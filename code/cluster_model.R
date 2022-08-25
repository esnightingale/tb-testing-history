################################################################################
################################################################################

library(tidyverse)
library(gtools)
library(ggmap)
library(sf)
library(patchwork)
library(brms)
library(spdep)
library(tidybayes)
library(sjPlot)
library(bayesplot)
library(gganimate)

# devtools::install_github("m-clark/visibly")
# library(visibly)

theme_set(theme_minimal())

figdir <- "figures/fit"

# Fix default ggsave background
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

#------------------------------------------------------------------------------#
# Run data loading script

source(here::here("code/load_data.R"), echo=TRUE)

#------------------------------------------------------------------------------#
# Map clusters and clinic areas

clinicarea$type = "Clinic"
clust$type = "Cluster"

clust %>% 
  bind_rows(clinicarea) %>% 
  mutate(type = factor(type, levels = c("Cluster", "Clinic"))) -> clust_clinic

clust_clinic %>% 
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() -> clust_cent

clust_clinic <- bind_cols(clust_clinic, clust_cent)

ggmap(blt_lines, 
      base_layer = ggplot(clust_clinic)) +
  geom_sf(aes(col = type, lty = type), fill = NA, lwd = 1) +
  geom_text(aes(label = clustid, x = X, y = Y)) +
  labs(x = "", y = "", col = "")
ggsave(here::here("figures","cluster_clinic_map.png"), height = 6, width = 6)

#------------------------------------------------------------------------------#
# Summarise missing data

dat %>% 
  dplyr::select(ever_test, agegp, sex, hiv_combined, pov_score, wealth_step) %>% 
  summary()
# ever_test    agegp          sex                 hiv_combined    pov_score        wealth_step   
# No :2305   18-24:1028   Female:1664   HIV positive ART: 292   Min.   :-23.619   Min.   :1.000  
# Yes: 433   25-34: 761   Male  :1074   HIV positive    :  37   1st Qu.: -6.951   1st Qu.:2.000  
#            35-44: 475                 HIV negative    :2292   Median : -4.858   Median :3.000  
#            45-54: 230                 HIV unknown     : 117   Mean   : -6.191   Mean   :2.917  
#            55-64: 123                                         3rd Qu.: -2.700   3rd Qu.:3.000  
#            65+  : 118                                         Max.   :  4.052   Max.   :6.000  
#            NA's :   3                                         NA's   :29        NA's   :29  

# Total incomplete observations
sum(is.na(dat$agegp) | dat$hiv_combined == "HIV unknown" | is.na(dat$pov_score))
# 148/2738 = 5.4%

dat_nona <- dat %>% 
  filter(!is.na(agegp) & hiv_combined != "HIV unknown" & !is.na(pov_score))

#------------------------------------------------------------------------------#
# Define relevant variables

cluster_inc <- prev_clust %>% 
  st_drop_geometry() %>% 
  dplyr::select(cluster_number, clust_inc_1518)

dat_nona$hiv <- dat_nona$hiv_combined
dat_nona$hiv[dat_nona$hiv == "HIV positive ART"] <- "HIV positive"

dat_nona <- mutate(dat_nona, 
              test01 = as.numeric(ever_test == "Yes"),
              agegp = factor(cut(s09age, breaks = c(17,24,34,44,54,max(dat$s09age, na.rm=T)))),
              hiv = factor(hiv, levels = c("HIV negative","HIV positive"), labels = c("HIV negative","HIV positive")),
              pov01 = as.factor(pov_score > 0),
              wealth_quant = as.factor(wealth_quant),
              wealth_quant1 = as.factor(wealth_quant == 1),
              wealth_step = as.factor(wealth_step),
              wealth_step1 = as.factor(wealth_step == 1),
              know_test = as.factor(s60tb_anyone == 1),
              know_trt = as.factor(s62tb_famtb == 1),
              know_died = as.factor(s65tb_died == 1)) %>% 
  full_join(cluster_inc, by = c("clustid" = "cluster_number"))

# Check
dat_nona %>% 
  dplyr::select(test01, sex, agegp, hiv, pov_score, pov01, wealth_quant, wealth_step, wealth_step1, clust_inc_1518, clinic_id) %>% 
  summary()

#------------------------------------------------------------------------------#
## Baseline model ##

# Fit a regression model for ever tested, adjusted for age, sex and HIV status
# with IID random effects by cluster. 

priors <- c(prior(normal(-2, 2), class = Intercept),
            prior(normal(0,10), class = b),
            prior(cauchy(0,1), class = sd, group = "clustid"))

fit_base <- brm(test01 ~ sex*agegp + hiv + (1 | clustid), 
                 data = dat_nona,
                 prior = priors,
                 family = "bernoulli")

pp_check(fit_base)

pdf(here::here(figdir, "condeff_base.pdf"), height = 5, width = 6)  
plot(conditional_effects(fit_base), ask = F)
dev.off()

summary(fit_base)
# ~clustid (Number of levels: 72) 
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.31      0.10     0.09     0.50 1.01     1082     1366
# 
# Population-Level Effects: 
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept         -2.67      0.12    -2.92    -2.43 1.00     2618     2350
# sexMale            0.21      0.12    -0.02     0.43 1.00     5544     3083
# agegp3040          0.95      0.15     0.64     1.25 1.00     4069     2941
# agegp4050          1.36      0.17     1.02     1.69 1.00     4264     3262
# agegp5087          1.66      0.16     1.34     1.98 1.00     4308     2986
# hivHIVpositive     1.06      0.15     0.77     1.35 1.00     5146     2983

#------------------------------------------------------------------------------#
# Plot fixed effects

plot_model(fit_base2, 
           order.terms = c(1,2,3,4,6,7,8,5),
           transform = NULL, 
           title = "",
           axis.labels = rev(c("Male","Age (30,40]", "Age (40,50]", "Age (50,87]",
                           "Male : Age (30,40]","Male : Age (40,50]", "Male : Age (50,87]",
                           "HIV positive"))) + 
  geom_hline(yintercept = 0, lty = "dashed", col = "grey")
ggsave(here::here(figdir,"fixeff_baseline.png"), height = 6, width = 6)

#------------------------------------------------------------------------------#
## Add poverty measures ##

# Explore relative significance of the two poverty measures
f1 <- test01 ~ sex + agegp + hiv + wealth_quant + (1 | clustid)
f2 <- test01 ~ sex + agegp + hiv + wealth_step + (1 | clustid)
f3 <- test01 ~ sex + agegp + hiv + pov_score + (1 | clustid)

# f3 <- test01 ~ sex*agegp + hiv + pov01 + (1 | clustid)
# f4 <- test01 ~ sex*agegp + hiv + wealth_quant1 + (1 | clustid)
# f5 <- test01 ~ sex*agegp + hiv + wealth_step1 + (1 | clustid)

f.list <- list(score_c = f3,
               score_q = f1, 
               step = f2
               # quant1 = f4,
               # step1 = f5
               )

fit_list_pov <- lapply(f.list, function(f) brm(f, data = dat_nona, prior = priors, family = "bernoulli"))
saveRDS(fit_list_pov, here::here("output","fit_list_pov.rds"))

# lapply(fit_list_pov, summary)
lapply(fit_list_pov, conditional_effects)

# Compare on LOOIC relative to baseline model
lapply(fit_list_pov, function(fit) LOO(fit, fit_base1))

get_loo <- function(fit, nm){
  est <- loo(fit)$estimates %>% 
    as.data.frame() %>% 
    rownames_to_column("metric") %>% 
    mutate(model = nm)
  return(est)
}
fits.all <- rlist::list.prepend(base = fit_base1, fit_list_pov)
lootab <- bind_rows(purrr::map2(fits.all, names(fits.all), get_loo))
lootab %>% 
  filter(metric == "looic") %>% 
  mutate(diff = min(Estimate) - Estimate) %>% 
  arrange(-diff) %>% 
  dplyr::select(model, Estimate, SE, diff)
#     model Estimate       SE       diff
# 1 score_c 2042.084 62.25087  0.0000000
# 2 score_q 2042.447 62.38537 -0.3628249
# 3    step 2043.266 62.31564 -1.1820880
# 4    base 2047.858 61.95493 -5.7737755

# Adding any measure of poverty seems preferable over the baseline model, but 
# self-assessed wealth appears to yield the clearest evidence of improvement. 

# Compare cluster random effects to baseline
clusteffs <- clusteffs %>% 
  mutate(model = "Baseline") %>% 
  bind_rows(
  as.data.frame(ranef(fit_list_pov)$clustid[,,1],
                model = "with poverty")
  )

ggplot(clusteffs, aes(x = 1:72, y = Estimate, ymin = `Q2.5`, ymax = `Q97.5`, col = model)) +
  geom_hline(yintercept = 0, col = "red", position = "dodge") +
  geom_point(position = position_dodge()) + 
  geom_errorbar() +
  labs(x = "Cluster")

clust %>% 
  full_join(clusteffs) -> clust

ggmap(blt_lines, 
      base_layer = ggplot(clust)) +
  geom_sf(aes(fill = Estimate), alpha = 0.5) +
  geom_sf(data = clinics, aes(pch = "TB Clinic")) +
  scale_shape_manual(values = 15) +
  scale_fill_gradient2(
    midpoint = 0, low = "red", mid = "white", high = "blue") +
  labs(x = "", y = "", 
       title = "Cluster level residuals",
       subtitle = "Adjusted for age group, sex and HIV status",
       fill = "Fitted value",
       lty = "", pch = "") 
ggsave(here::here("figures","cluster_IID_baseline.png"), height = 6, width = 6)

MC <- moran.mc(clusteffs$Estimate, lw, nsim=999, alternative = "less")
MC
plot(MC, main="", las=1)


#------------------------------------------------------------------------------#
# Check spatial autocorrelation in fitted cluster IID effects

clusteffs <- as.data.frame(ranef(fit_base2)$clustid[,,1]) %>% 
  mutate(clustid = 1:72)

ggplot(clusteffs, aes(x = 1:72, y = Estimate, ymin = `Q2.5`, ymax = `Q97.5`)) +
  geom_hline(yintercept = 0, col = "red") +
  geom_point() + 
  geom_errorbar() +
  labs(x = "Cluster")

clust %>% 
  full_join(clusteffs) -> clust

ggmap(blt_lines, 
      base_layer = ggplot(clust)) +
  geom_sf(aes(fill = Estimate), alpha = 0.5) +
  # geom_sf(data = clinics, aes(pch = "TB Clinic")) +
  scale_shape_manual(values = 15) +
  scale_fill_gradient2(
    midpoint = 0, low = "red", mid = "white", high = "blue") +
  theme(axis.text = element_blank()) +
  labs(x = "", y = "", 
       title = "Cluster level residuals",
       subtitle = "Adjusted for age group, sex and HIV status",
       fill = "Fitted value",
       lty = "", pch = "",
       caption = paste0("Excluding N = ",nrow(dat) - nrow(dat_nona)," observations with missing values.")) 
ggsave(here::here(figdir,"cluster_IID_baseline.png"), height = 6, width = 6)

# Specify cluster neighbour structure
nb <- poly2nb(clust, snap = 0.001)

# Cluster 31 has no neighbours - impute as 27 for completeness
nb[[31]] = as.integer(c(27,28))

lw <- nb2listw(nb, style="B", zero.policy=TRUE)
M <- nb2mat(nb, style = "B", zero.policy = TRUE)

MC <- moran.mc(clusteffs$Estimate, lw, nsim=999, alternative = "less")
MC
# statistic = -0.23123, observed rank = 2, p-value = 0.002
png(here::here("figures/fit","moran_IID_baseline.png"), height = 400, width = 500)
plot(MC, main="", las=1)
dev.off()

#------------------------------------------------------------------------------#
# Plot IID effects against spatial covariates

dat_nona %>% 
  group_by(clustid) %>% 
  summarise(n = n(),
            perc_tested = 100*sum(ever_test == "Yes")/n,
            perc_pmt_gt0 = 100*sum(pov_score > 0)/n,
            perc_quant1 = 100*sum(wealth_quant == 1)/n,
            perc_step1 = 100*sum(wealth_step == 1)/n,
            perc_know_test = 100*sum(s60tb_anyone == 1)/n,
            perc_know_trt = 100*sum(s62tb_famtb == 1)/n,
            perc_know_died = 100*sum(s65tb_died == 1)/n) %>% 
  full_join(clusteffs) -> dat_by_clust

dat_by_clust %>% 
  pivot_longer(perc_pmt_gt0:perc_know_died) %>% 
  ggplot(aes(value/100, Estimate)) +
  geom_point(alpha = 0.5) +
  # geom_smooth() +
  scale_x_continuous(trans = "logit") +
  facet_wrap(~name) +
  labs(x = "Cluster percentage", y = "Fitted IID effect")

dat_by_clust %>% 
  pivot_longer(perc_pmt_gt0:perc_know_died) %>%
  ggplot(aes(value, perc_tested)) +
  geom_point(alpha = 0.5) +
  # geom_smooth() +
  # scale_x_continuous(trans = "logit") +
  facet_wrap(~name) +
  labs(x = "Cluster percentage", y = "Percentage ever tested")

dat_nona %>% 
  full_join(clusteffs) %>% 
  pivot_longer(c("wealth_quant","wealth_step","pov01","wealth_quant1",
                 "wealth_step1", "know_test","know_trt","know_died"),
               values_transform = as.factor) %>%
  ggplot(aes(x = value, y = Estimate)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free_x")
#------------------------------------------------------------------------------#
# Add previous case notification rate per cluster 

# f_previnc <- test01 ~ sex*agegp2 + hiv_combined + as.numeric(wealth_quant) + clust_inc_1518 +
#   f(s02cl_id, model = "bym2", 
#     graph = G, 
#     hyper = priors.bym,
#     scale.model = TRUE, constr = TRUE)
# 
# # Fit
# fit_previnc <- fit_inla(f_previnc)

#------------------------------------------------------------------------------#
# Save fitted models

fits_all <- list(base = fit_base,
                 score_num = fit.list.pov$score_num,
                 pov_factor = fit.list.pov$pov_factor,
                 wealth_factor = fit.list.pov$wealth_factor)
saveRDS(fits_all, here::here("testing history","output","fits_all.rds"))

################################################################################
################################################################################