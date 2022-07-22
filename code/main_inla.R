################################################################################
################################################################################

library(tidyverse)
library(magrittr)
library(ggmap)
library(sf)
library(raster)
library(ggregplot)
library(BlantyreTBEpi)
library(patchwork)
library(INLA)

theme_set(theme_minimal())

figdir <- "testing history/figures/fit"

# Fix default ggsave background
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

# Source model coeff plot
source(here::here("testing history/code/utils","Efxplot.R"), echo=TRUE)
source(here::here("testing history/code/utils","fit_inla.R"), echo=TRUE)

# Function to extract/tabulate fixed effect estimates
transform_coeff <- function(marg){
  emarg <- inla.tmarginal(exp, marg)
  zmarg <- inla.zmarginal(emarg, silent = T)
  summ.coeff <- setNames(t(zmarg), names(zmarg))
  return(summ.coeff)
}
get_coeffs <- function(fit){
  coeftab <- purrr::map_dfr(fit$marginals.fixed, transform_coeff) %>%
    mutate(effect = names(fit$marginals.fixed), .before = everything())
  return(coeftab)
}

#------------------------------------------------------------------------------#
# Run data loading script

source(here::here("testing history/code","load_data.R"), echo=TRUE)

#------------------------------------------------------------------------------#
# Check/define variables for modelling

summary(dat_all$pov_score)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -23.619  -6.981  -4.892  -6.246  -2.633   4.063     107 

summary(dat_all$pov01)
# Mode   FALSE    TRUE    NA's 
# logical   14952     838     107 

summary(as.factor(dat_all$pov_quant))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  1.0     2.0     3.0     3.5     5.0     6.0     107 

ggplot(dat_all, aes(pov_score, fill = as.factor(pov_quant))) +
  geom_histogram()

ggplot(dat, aes(as.factor(pov_quant), fill = test_history)) +
  geom_bar()

dat_all %>% 
  filter(!is.na(pov_score)) %>% 
  ggplot(aes(as.factor(pov_step), pov_score)) +
  geom_hline(yintercept = 0, col = "grey", lty = "dashed") +
  geom_boxplot() +
  labs(x = "Self-assessed wealth", y= "PMT poverty score")
ggsave(here::here("testing history/figures/poverty","pov_score_byself.png"), 
       height = 5, width = 6)

ggplot(dat_all, aes(pov_score, fill = as.factor(pov_step))) +
  geom_histogram() +
  labs(x = "PMT poverty score", y = "Count", fill = "Self-assessed wealth") +
  theme(legend.position = c(0.2,0.8))
ggsave(here::here("testing history/figures/poverty","pov_score_byself_hist.png"), 
       height = 5, width = 7)

dat_all %>% 
  group_by(s02cl_id) %>% 
  summarise(n = n(),
            x = sum(test_history == "Yes"),
            perc_test_history = 100*x/n, # Hmisc::binconf(x, n)[1],
            mean_score = mean(pov_score, na.rm = T),
            mean_sa = mean(pov_step, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(score = scale(mean_score),
         self = scale(mean_sa)) -> dat_clust

ggplot(dat_clust, aes(mean_score, 
                      mean_sa)) +
  # geom_abline(slope = -1, lty = "dashed") +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm") + 
  labs(x = "Cluster mean probability of poverty", y = "Cluster mean self-assessed wealth")
ggsave(here::here("testing history/figures/poverty","cluster_score_vs_sa.png"), 
       height = 5, width = 6)

#------------------------------------------------------------------------------#
# Subset to extended questionnaire

dat <- dat_all %>% 
  filter(incl_extended)

mean(dat$s50tb_medtest == 1) #0.1004383
mean(dat$s53tb_evrxray == 1) #0.09934259
mean(dat$s50tb_medtest == 1 & dat$s53tb_evrxray == 1)

# Exclude 29 with missing household information (3 also missing age)
dat <- dat %>%
  filter(!is.na(pov_score) & !is.na(agegp))

#------------------------------------------------------------------------------#

# Define relevant variables
cluster_inc <- prev_clust %>% 
  st_drop_geometry() %>% 
  dplyr::select(cluster_number, clust_inc_1518)
dat$hiv <- dat$hiv_combined
dat$hiv[dat$hiv == "HIV positive ART"] <- "HIV positive"
dat <- mutate(dat, 
              test01 = as.numeric(test_history == "Yes"),
              agegp2 = cut(s09age, breaks = c(17,30,40,50,max(dat$s09age, na.rm=T))),
              hiv = factor(hiv, levels = c("HIV negative","HIV positive","HIV unknown")),
              pov_quant = factor(pov_quant, levels = c(6,5,4,3,2,1)),
              wealth_step = factor(wealth_step, levels = c(1,2,3,4,5,6))) %>% 
  full_join(cluster_inc, by = c("s02cl_id" = "cluster_number"))

# Check
dat %>% 
  dplyr::select(test01, sex, agegp, agegp2, hiv, pov_score, pov01, pov_score_exp, pov_quant, wealth_step, clust_inc_1518) %>% 
  summary()

plot_bars_byperc(dat, "pov_quant","test_history") +
  labs(x = "Poverty score quantile", y = "%", fill = "Ever tested") +
  guides(fill = "none") -> bypov

plot_bars_byperc(dat, "wealth_step","test_history") +
  labs(x = "Self-assessed wealth", y = "", fill = "Ever tested") -> bywealth

bypov + bywealth
ggsave(here::here("testing history/figures/poverty","testing_byscore_self.png"), 
       height = 5, width = 10)

#------------------------------------------------------------------------------#
## Baseline model ##

# Fit a regression model for ever tested, adjusted for age, sex and HIV status
# with IID random effects by cluster.

## Define cluster adjacency graph
clust.nb <- spdep::poly2nb(clust, snap = 0.01)
G <- spdep::nb2mat(clust.nb, style = "B", zero.policy = TRUE) 

# Prior for IID precision
prior.prec <- list(prec = list(prior = "pc.prec", param = c(5, 0.01)))
## Priors for BYM precision and phi
priors.bym <- list(prec = list(prec = list(prior = "pc.prec", param = c(0.5/0.31, 0.01))),
                   phi = list(param = c(0.5, 2/3)))

# Define linear combination for men aged 50+
older_men <- inla.make.lincomb(sexMale = 1, `agegp2(50,87]` = 1, `sexMale:agegp2(50,87]` = 1 )

# Formulae
f_base1 <- test01 ~ sex + agegp2 + hiv + f(s02cl_id, model = "bym2", graph = G, hyper = priors.bym, scale.model = TRUE, constr = TRUE)
  
  # f(s02cl_id, model = "iid", hyper = prior.prec)

f_base2 <- test01 ~ sex*agegp2 + hiv + f(s02cl_id, model = "bym2", graph = G, hyper = priors.bym, scale.model = TRUE, constr = TRUE)

# Fit baseline and age-sex interaction models
fit_base1 <- fit_inla(f_base1)
summary(fit_base1)
# Fixed effects:
#                   mean    sd 0.025quant 0.5quant 0.975quant   mode kld
# (Intercept)     -2.665 0.118     -2.904   -2.663     -2.440 -2.658   0
# sexMale          0.222 0.117     -0.007    0.222      0.450  0.222   0
# agegp2(30,40]    0.944 0.147      0.656    0.944      1.232  0.944   0
# agegp2(40,50]    1.406 0.166      1.079    1.406      1.731  1.407   0
# agegp2(50,87]    1.686 0.160      1.371    1.686      1.999  1.686   0
# hivHIV positive  1.036 0.142      0.757    1.037      1.314  1.037   0
# hivHIV unknown  -0.186 0.302     -0.809   -0.175      0.379 -0.155   0
# 
# Random effects:
#   Name	  Model
# s02cl_id BYM2 model
# 
# Model hyperparameters:
#                          mean     sd 0.025quant 0.5quant 0.975quant  mode
# Precision for s02cl_id 22.467 22.043      5.012    15.81     79.944 9.540
# Phi for s02cl_id        0.152  0.151      0.007     0.10      0.576 0.016
# 
# Deviance Information Criterion (DIC) ...............: 2125.93
# Deviance Information Criterion (DIC, saturated) ....: -39992.45
# Effective number of parameters .....................: 21.25
# 
# Watanabe-Akaike information criterion (WAIC) ...: 2125.56
# Effective number of parameters .................: 20.67
# 
# Marginal log-Likelihood:  -1042.04 

fit_base2 <- fit_inla(f_base2, lincomb = older_men)
summary(fit_base2)
# Fixed effects:
#                         mean    sd 0.025quant 0.5quant 0.975quant   mode kld
# (Intercept)           -2.515 0.131     -2.780   -2.512     -2.267 -2.506   0
# sexMale               -0.130 0.200     -0.526   -0.128      0.258 -0.125   0
# agegp2(30,40]          0.821 0.178      0.471    0.821      1.171  0.820   0
# agegp2(40,50]          1.278 0.206      0.873    1.278      1.682  1.279   0
# agegp2(50,87]          1.194 0.225      0.747    1.196      1.631  1.199   0
# hivHIV positive        1.008 0.142      0.727    1.008      1.286  1.008   0
# hivHIV unknown        -0.209 0.306     -0.839   -0.198      0.363 -0.178   0
# sexMale:agegp2(30,40]  0.291 0.306     -0.312    0.292      0.889  0.293   0
# sexMale:agegp2(40,50]  0.325 0.333     -0.330    0.325      0.977  0.326   0
# sexMale:agegp2(50,87]  1.041 0.325      0.406    1.040      1.684  1.037   0
# 
# Linear combinations (derived):
#    ID  mean    sd 0.025quant 0.5quant 0.975quant  mode kld
# lc  1 2.105 0.216      1.683    2.105      2.529 2.105   0
# 
# Random effects:
#   Name	  Model
# s02cl_id BYM2 model
# 
# Model hyperparameters:
#                          mean     sd 0.025quant 0.5quant 0.975quant  mode
# Precision for s02cl_id 22.893 22.818      5.003   15.996     82.300 9.576
# Phi for s02cl_id        0.153  0.152      0.007    0.101      0.579 0.016
# 
# Deviance Information Criterion (DIC) ...............: 2121.53
# Deviance Information Criterion (DIC, saturated) ....: -39996.86
# Effective number of parameters .....................: 23.90
# 
# Watanabe-Akaike information criterion (WAIC) ...: 2121.39
# Effective number of parameters .................: 23.45
# 
# Marginal log-Likelihood:  -1050.79 

get_coeffs(fit_base2)
# effect                    mean     sd quant0.025 quant0.25 quant0.5 quant0.75 quant0.975
# 1 (Intercept)           0.0816 0.0105     0.0621    0.0742   0.0811    0.0884      0.103
# 2 sexMale               0.896  0.178      0.592     0.768    0.879     1.01        1.29 
# 3 agegp2(30,40]         2.31   0.412      1.60      2.01     2.27      2.56        3.22 
# 4 agegp2(40,50]         3.67   0.758      2.40      3.12     3.59      4.12        5.36 
# 5 agegp2(50,87]         3.38   0.762      2.12      2.84     3.30      3.84        5.10 
# 6 hivHIV positive       2.77   0.393      2.07      2.49     2.74      3.01        3.61 
# 7 hivHIV unknown        0.850  0.256      0.433     0.665    0.820     1.00        1.43 
# 8 sexMale:agegp2(30,40] 1.40   0.434      0.734     1.09     1.34      1.64        2.42 
# 9 sexMale:agegp2(40,50] 1.46   0.495      0.721     1.11     1.38      1.73        2.65 
# 10 sexMale:agegp2(50,87] 2.98   0.993      1.51      2.27     2.83      3.52        5.37 

# Summarise the exponentiated lincomb for older men 
inla.zmarginal(inla.tmarginal(function(x) exp(x), fit_base2$marginals.lincomb.derived$lc))

# Mean            8.4013 
# Stdev           1.81941 
# Quantile  0.025 5.38969 
# Quantile  0.25  7.09726 
# Quantile  0.5   8.2066 
# Quantile  0.75  9.49026 
# Quantile  0.975 12.5057 

#------------------------------------------------------------------------------#
## Plot fitted fixed and random effects ##

c("Intercept",
  "Male",
  "Age (30, 40]",
  "Age (40, 50]",
  "Age (50, 87]",
  "HIV positive",
  "HIV unknown",
  "Male:age (30, 40]",
  "Male:age (40, 50]",
  "Male:age (50, 87]") -> axis.labs

Efxplot(list(`Without interaction` = fit_base1, `With interaction` = fit_base2), 
        Intercept = F,
        VarNames = rev(axis.labs)) +
  theme(legend.position = c(0.8,0.3))
ggsave(here::here(figdir,"base_fixed_effects.png"), 
       height = 5, width = 6)

fit_base <- fit_base2

clust %>% 
  # mutate(raneff = fit_base$summary.random$s02cl_id$mean[1:nrow(clust)],
  #        bym_besag = fit_base$summary.random$s02cl_id$mean[1:nrow(clust)+nrow(clust)]) -> clust
  mutate(raneff = fit_base$summary.random$s02cl_id$mean[1:nrow(clust)]) -> clust

ggmap(blt_lines, 
      base_layer = ggplot(clust)) +
  geom_sf(aes(fill = raneff), alpha = 0.5) +
  scale_fill_gradient2(
    midpoint = 0, low = "red", mid = "white", high = "blue") +
  # scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(x = "", y = "", 
       title = "Fitted random effect per cluster, adjusting for age, sex and HIV status",
       fill = "Mean",
       lty = "", pch = "")

ggsave(here::here(figdir,"base_fitted_cluster_reff.png"), 
       height = 5, width = 7)

#------------------------------------------------------------------------------#
## Add poverty measures ##

dat$pov_quant2 <- relevel(dat$pov_quant, ref = 4)
dat$wealth_step2 <- relevel(dat$wealth_step, ref = 3)

# Explore relative significance of the two poverty measures
f1 <- test01 ~ sex*agegp2 + hiv + pov_score + f(s02cl_id, model = "bym2", graph = G, hyper = priors.bym, scale.model = TRUE, constr = TRUE)
# f2 <- test01 ~ sex*agegp2 + hiv + pov01 + f(s02cl_id, model = "bym2", graph = G, hyper = priors.bym, scale.model = TRUE, constr = TRUE)
f3 <- test01 ~ sex*agegp2 + hiv + pov_quant2 + f(s02cl_id, model = "bym2", graph = G, hyper = priors.bym, scale.model = TRUE, constr = TRUE)
f4 <- test01 ~ sex*agegp2 + hiv + wealth_step2 + f(s02cl_id, model = "bym2", graph = G, hyper = priors.bym, scale.model = TRUE, constr = TRUE)

f.list <- list(score_num = f1, 
               # score_gt0 = f2,
               quant_factor = f3, 
               step_factor = f4)
fit.list.pov <- lapply(f.list, fit_inla, lincomb = older_men)

# fit1 <- fit_inla(f1, lincomb = older_men)
# fit2 <- fit_inla(f2, lincomb = older_men)
# fit3 <- fit_inla(f3a, lincomb = NULL)
# fit4 <- fit_inla(f4a, lincomb = NULL)

# fit.list.pov <- list(score_num = fit.list.pov$score_num,
#                      pov_factor = fit3,
#                      wealth_factor = fit4)

lapply(fit.list.pov, summary)

# Extract coefficient estimates for comparison/plotting:
coeftab <- bind_rows(lapply(fit.list.pov, get_coeffs)) 

coeftab %>% 
  filter(grepl("pov_", effect)|grepl("wealth_", effect)) %>%
  mutate(across(mean:`quant0.975`, function(x) round(x, 2)),
         estimate = paste0(mean, " (",`quant0.025`,"-",`quant0.975`,")")) %>% 
  dplyr::select(effect, estimate) -> coeftab
coeftab
# effect       estimate        
# 1 pov_score     0.98 (0.96-1)   
# 2 pov_quant26   0.62 (0.4-0.92) 
# 3 pov_quant25   0.87 (0.59-1.23)
# 4 pov_quant24   0.8 (0.54-1.15) 
# 5 pov_quant22   1.13 (0.78-1.59)
# 6 pov_quant21   1.09 (0.74-1.56)
# 7 wealth_step21 0.63 (0.37-0.98)
# 8 wealth_step22 0.71 (0.52-0.94)
# 9 wealth_step24 1.23 (0.92-1.61)
# 10 wealth_step25 0.96 (0.48-1.66)
# 11 wealth_step26 0.82 (0.17-2.11)
write.csv(coeftab, here::here("testing history/output", "compare_poverty_coeffs.csv"))

# Compare on WAIC relative to baseline model
waic.list <- sapply(fit.list.pov, function(x) x$waic$waic)
waic.list - fit_base$waic$waic
# score_num    pov_factor wealth_factor 
# -2.365603     -1.574063     -3.570614  

dic.list <- sapply(fit.list.pov, function(x) x$dic$dic)
dic.list - fit_base$dic$dic
# score_num    pov_factor wealth_factor 
# -2.283176     -1.931244     -4.161627 

# Adding any measure of poverty seems preferable over the baseline model, but 
# self-assessed wealth appears to yield the clearest evidence of improvement. 

# Plot coefficient estimates:
c("Intercept",
  "Male",
  "Age (30, 40]",
  "Age (40, 50]",
  "Age (50, 87]",
  "HIV positive",
  "HIV unknown",
  "Poverty score",
  "Male:age (30, 40]",
  "Male:age (40, 50]",
  "Male:age (50, 87]",
  "Poverty quantile: 6",
  "Poverty quantile: 5",
  "Poverty quantile: 4",
  "Poverty quantile: 2",
  "Poverty quantile: 1  (least poor)",
  "Wealth step: 1",
  "Wealth step: 2",
  "Wealth step: 4",
  "Wealth step: 5",
  "Wealth step: 6 (least poor)") -> axis.labs2

Efxplot(fit.list.pov, 
        Intercept = F,
        VarSubset = c("Poverty score",
                      "Poverty quantile: 6",
                      "Poverty quantile: 5",
                      "Poverty quantile: 4",
                      "Poverty quantile: 2",
                      "Poverty quantile: 1  (least poor)",
                      "Wealth step: 1",
                      "Wealth step: 2",
                      "Wealth step: 4",
                      "Wealth step: 5",
                      "Wealth step: 6 (least poor)"),
        VarNames = rev(axis.labs2)
        ) + 
  theme(legend.position = "none")
ggsave(here::here(figdir,"povscores_fixed_effects.png"), 
       height = 5, width = 6)

# Define final fit
fit_final <- fit.list.pov$wealth_factor

clust %>% 
  mutate(bym_final = fit_final$summary.random$s02cl_id$mean[1:nrow(clust)],
         bym_besag_final = fit_final$summary.random$s02cl_id$mean[1:nrow(clust)+nrow(clust)]) -> clust

ggmap(blt_lines, 
      base_layer = ggplot(clust)) +
  geom_sf(aes(fill = bym_final), alpha = 0.5) +
  geom_sf(data = clinics, aes(pch = "TB Clinic")) +
  scale_shape_manual(values = 15) +
  scale_fill_gradient2(
    midpoint = 0, low = "red", mid = "white", high = "blue") +
  # scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(x = "", y = "", 
       title = "Total spatial residual",
       subtitle ="(spatial + non-spatial effects)",
       fill = "Fitted value",
       lty = "", pch = "") #-> map_bym

ggsave(here::here(figdir,"fitted_bym_final.png"), height = 6, width = 6)

ggmap(blt_lines, 
      base_layer = ggplot(clust)) +
  geom_sf(aes(fill = bym_besag_final), alpha = 0.5) +
  geom_sf(data = clinics, aes(pch = "TB Clinic")) +
  scale_shape_manual(values = 15) +
  scale_fill_gradient2(
    midpoint = 0, low = "red", mid = "white", high = "blue") +
  # scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(x = "", y = "", 
       title = "Fitted spatial random effect",
       subtitle = "",
       fill = "Fitted value",
       lty = "", pch = "") #-> map_besag

ggsave(here::here(figdir,"fitted_bym_besag_final.png"), height = 6, width = 6)

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