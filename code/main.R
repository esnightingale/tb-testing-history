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

theme_set(theme_minimal())

figdir <- "testing history/figures"

# Fix default ggsave background
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

# Source model coeff plot
source(here::here("testing history","code","Efxplot.R"), echo=TRUE)

################################################################################
## LOAD DATA ##

#-----------#
# Geography #

# Cluster/HSA polygons and clinic locations
clust <- readRDS(here::here("data","clusters.rds")) %>% # Survey clusters
  mutate(clustid = as.numeric(gsub("c","",cluster)))
data("clinics", package="BlantyreTBEpi") # TB clinics
data("hsas", package="BlantyreTBEpi") # Health service areas

# Update CRS type
clust <- st_transform(clust, st_crs(4326))
clinics <- st_transform(clinics, st_crs(4326))
hsas <- st_transform(hsas, st_crs(4326))

# Extract a base map layer to the extent of cluster polygons
bb <- st_bbox(clust)
e <- c(bb$xmin, bb$ymin, bb$xmax, bb$ymax)
names(e) <- c("left","bottom","right","top")
blt_lines <- get_map(location = e, source = "stamen", maptype = "terrain", zoom = 12)

#------------#
# Population #

# Worldpop Malawi population count raster
pop.path <- here::here("data","mwi_ppp_2020_UNadj.tif")
popMLW <- raster::raster(x = pop.path)

# Crop to same map extent
popBLT <- raster::crop(popMLW, raster::extent(bb))
# plot(popBLT)

# Reformat for later plotting
popBLT_spdf <- as(popBLT, "SpatialPixelsDataFrame")
popBLT_df <- as.data.frame(popBLT_spdf)

#-------------------------#
# Routine TB surveillance #

data("cnrs", package="BlantyreTBEpi")    # Case notifications per HSA
cnrs <- st_transform(cnrs, st_crs(4326))


data("cluster_cases_sf", package="BlantyreTBEpi")    # Case notifications per HSA
tb_hsa <- hsas %>% 
  full_join(cluster_cases_sf)

#-------------------#
# Prevalence survey #

# Cluster prevalence (from Mcewen's paper)
prev_clust <- readRDS(here::here("data","dat_scale.rds"))

# Individual data
dat_all <- readRDS("C:/Users/phpuenig/Dropbox/SCALE_Hit-TB_modelling-resources/ModellingData/Pre_PS_Data/survey_clean.rds") %>% 
  dplyr::select(today, ind_id, s02cl_id, 
                # Household data
                hh_id, gps_lat, gps_lng, gps_alt, gps_acc, hh_per_room, pov_score,h22hh_step, 
                # Individual data
                s07sex, sex, s09age, agegp, hiv_combined,
                # Current TB 
                any_cough, any_tb_symp,
                # Known TB
                s60tb_anyone, s62tb_famtb, s65tb_died,
                
                ## EXTENDED ONLY ##
                
                # Ever TB test/diagnosis
                s50tb_medtest, s51tb_spnum, s53tb_evrxray, s54tb_xraynum, 
                s56tb_evrtst, 
                # self-assessed sick, current trt
                s30tb_sick, s57tb_treat,
                # Service usage
                s111serv_hosp, s111serv_hosp_period) %>%
  mutate(wealth_quant = ntile(pov_score, 6),
         wealth_cut = cut(pov_score, 6),
         any_tb_symp = as.factor(any_tb_symp),
         current_trt = factor((s57tb_treat == 1), 
                              levels = c(FALSE,TRUE), 
                              labels = c("No","Yes")),
         incl_extended = !is.na(s50tb_medtest),
         test_history = factor((s50tb_medtest == 1 | s53tb_evrxray == 1), 
                               levels = c(FALSE,TRUE), 
                               labels = c("No","Yes"))) %>% 
  # Count participants per cluster
  group_by(s02cl_id) %>% 
  mutate(clust_N_tot = n(),
         clust_N_ext = sum(incl_extended)) %>% 
  ungroup() 

summary(dat_all$pov_score)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -23.619  -6.981  -4.892  -6.246  -2.633   4.063     107 

summary(dat_all$wealth_quant)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  1.0     2.0     3.0     3.5     5.0     6.0     107 

ggplot(dat_all, aes(pov_score, fill = as.factor(wealth_quant))) +
  geom_histogram()
ggplot(dat_all, aes(pov_score, fill = wealth_cut)) +
  geom_histogram()

# Subset to extended questionnaire
dat <- dat_all %>% 
  filter(incl_extended)

#------------------------------------------------------------------------------#
## Simple cluster model ##

# Fit a simple regression model for ever tested, adjusted for age and sex
# with IID random effects by cluster.

dat <- mutate(dat, 
              test01 = as.numeric(test_history == "Yes"),
              agegp2 = cut(s09age, breaks = c(17,30,40,50,max(dat$s09age, na.rm=T))),
              hiv_combined = relevel(hiv_combined, ref = "HIV negative"),
              wealth_quant = factor(wealth_quant, levels = c(3,1,2,4,5,6)),
              h22hh_step = factor(h22hh_step, levels = c(3,6,5,4,2,1)))

dat %>% 
  dplyr::select(test01, sex, agegp, agegp2, hiv_combined, wealth_quant, h22hh_step) %>% 
  summary()

# Exclude 29 with missing household information 
# dat <- dat %>% 
#   filter(!is.na(wealth_quant) & !is.na(agegp2))

# Exclude 3 with missing age
dat <- dat %>%
  filter(!is.na(agegp2))

fit_base <- lme4::glmer(test01 ~ sex + agegp2 + hiv_combined + (1|s02cl_id),
                        data = dat,
                        family = "binomial")
summary(fit_base)

fit_base2 <- lme4::glmer(test01 ~ sex*agegp2 + hiv_combined + (1|s02cl_id),
                         data = dat,
                         family = "binomial")
summary(fit_base2)

#------------------------------------------------------------------------------#
## Plot fitted IID effects ##

clust %>% 
  mutate(clust_iid = lme4::ranef(fit_base2)$s02cl_id[,1]) -> clust

ggmap(blt_lines, 
      base_layer = ggplot(clust)) +
  geom_sf(aes(fill = clust_iid), alpha = 0.5) +
  # geom_sf(data = clinics, aes(pch = "TB Clinic")) +
  # scale_shape_manual(values = 15) +
  scale_fill_gradient2(
    midpoint = 0, low = "blue", mid = "white", high = "red") +
  # scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(x = "", y = "", 
       fill = "Fitted value",
       lty = "", pch = "", 
       title = "Fitted IID residual - adjusted for age, sex and HIV status"
       # subtitle = ""
       ) 

ggsave(here::here(figdir,"base_fitted_cluster_iid.png"), 
       height = 5, width = 6)

#------------------------------------------------------------------------------#
## Add poverty measures ##

# Explore relative significance of the two poverty measures

fit_pov1 <- lme4::glmer(test01 ~ sex*agegp2 + hiv_combined + wealth_quant + (1|s02cl_id),
                        data = dat,
                        family = "binomial")

fit_pov2 <- lme4::glmer(test01 ~ sex*agegp2 + hiv_combined + h22hh_step + (1|s02cl_id),
                        data = dat,
                        family = "binomial")

# As numeric vars
fit_pov1a <- lme4::glmer(test01 ~ sex*agegp2 + hiv_combined + as.numeric(wealth_quant) + (1|s02cl_id),
                         data = dat,
                         family = "binomial")

fit_pov2a <- lme4::glmer(test01 ~ sex*agegp2 + hiv_combined + as.numeric(h22hh_step) + (1|s02cl_id),
                         data = dat,
                         family = "binomial")

fit.list <- list(quant_factor = fit_pov1, quant_num = fit_pov1a, step_factor = fit_pov2, step_num = fit_pov2a)
lapply(fit.list, summary)

plot_coefs <- function(fit){
  df <- broom.mixed::tidy(fit, conf.int = T, exponentiate = T) %>% 
    mutate(term = factor(term, levels = term)) %>% 
    mutate(term2 = as.factor(gsub("sex","ex:",
                                  gsub("agegp2","age:",
                                       gsub("hiv_combined","hiv_combined:",
                                            gsub("wealth_quant","wealth_quant:",
                                                 gsub("h22hh_step","h22hh_step:",term))))))) %>%
    separate(term2, into = c("var","value"),":", remove = FALSE)
  
  df %>% 
    filter(effect == "fixed") %>% 
    ggplot(aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high)) +
    geom_vline(xintercept = 1, col = "grey", lty = "dashed") +
    geom_errorbarh(height= 0.3) +
    geom_point() +
    guides(col = "none") +
    scale_y_discrete(limits=rev) +
    # coord_flip() +
    labs(x = "Estimate",y = "") -> p
  
  return(p)
}
lapply(fit.list, plot_coefs)

lapply(fit.list, function(fit) anova(fit, fit_base2, test="Chisq"))

# Adding any measure of poverty seems preferable over the baseline model, but wealth quantile included as a linear effect
# appears yield the clearest evidence of improvement. 

dat_prev_clust %>% 
  dplyr::select(cluster_number, prev_samp) %>% 
  mutate(clust_iid = lme4::ranef(fit.list$quant_num)$s02cl_id[,1],
         prev_samp_cut = gtools::quantcut(prev_samp, 4)) -> dat_prev_clust

summary(dat_prev_clust$prev_samp_cut)

dat_prev_clust %>% 
  ggplot(aes(prev_samp_cut, clust_iid)) +
  geom_boxplot()

################################################################################

## Spatially-structured model ##

# Fit alternative models with both spatially structured and unstructured
# random components, using INLA.

## Define cluster adjacency graph
clust.nb <- spdep::poly2nb(clust, snap = 0.01)
G <- spdep::nb2mat(clust.nb, style = "B", zero.policy = TRUE) 

## Priors
# For IID precision
prior.prec <- list(prec = list(prior = "pc.prec", param = c(5, 0.01)))
# For BYM precision and phi
priors.bym <- list(prec = list(prec = list(prior = "pc.prec", param = c(0.5/0.31, 0.01))),
                   phi = list(param = c(0.5, 2/3)))

## Formulae
f1 <- test01 ~ sex*agegp2 + hiv_combined + as.numeric(wealth_quant) + f(s02cl_id, model = "iid",
                                                                        hyper = prior.prec)
f2 <- test01 ~ sex*agegp2 + hiv_combined + as.numeric(wealth_quant) + f(s02cl_id, model = "bym2", 
                                                                        graph = G, 
                                                                        hyper = priors.bym,
                                                                        scale.model = TRUE, constr = TRUE)

## Fitting
fit_model <- function(f){
  fit <- INLA::inla(f, 
                    data = dat,
                    family = "binomial",
                    control.compute = list(waic = TRUE, dic = TRUE),
                    control.predictor = list(compute = TRUE),
                    verbose = FALSE)
  return(fit)
}
fit.list2 <- lapply(list(f1, f2), fit_model)


# Assess the added value of the spatial structure by comparison of WAIC:

lapply(fit.list2, summary)

# The posterior mean for the IID precision in the non-spatial model is
# large and very uncertain =\> little variability in this posterior. Adding
# the spatial component reduces this.The posterior mean for phi suggests
# around 19% of the residual variability may be explained by the spatial
# structure. This estimate is also quite uncertain however.

sapply(fit.list2, function(x) x$waic$waic)

Efxplot(fit.list2, Intercept = FALSE)

#------------------------------------------------------------------------------#
## Plot fitted IID/BYM effects ##

clust %>% 
  mutate(iid = fit.list2[[1]]$summary.random$s02cl_id$mean,
         bym = fit.list2[[2]]$summary.random$s02cl_id$mean[1:nrow(clust)],
         bym_besag = fit.list2[[2]]$summary.random$s02cl_id$mean[1:nrow(clust)+nrow(clust)]) -> clust

ggmap(blt_lines, 
      base_layer = ggplot(clust)) +
  geom_sf(aes(fill = iid, lty = "Survey cluster"), alpha = 0.5) +
  geom_sf(data = clinics, aes(pch = "TB Clinic")) +
  scale_shape_manual(values = 15) +
  scale_fill_gradient2(
    midpoint = 0, low = "blue", mid = "white", high = "red") +
  # scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(x = "", y = "", 
       title = "Fitted IID random effect",
       fill = "Fitted value",
       lty = "", pch = "", 
       # title = "",
       subtitle = "") #-> map_iid

ggsave(here::here(figdir,"fit","fitted_iid.png"), height = 6, width = 6)


ggmap(blt_lines, 
      base_layer = ggplot(clust)) +
  geom_sf(aes(fill = bym, lty = "Survey cluster"), alpha = 0.5) +
  geom_sf(data = clinics, aes(pch = "TB Clinic")) +
  scale_shape_manual(values = 15) +
  scale_fill_gradient2(
    midpoint = 0, low = "blue", mid = "white", high = "red") +
  # scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(x = "", y = "", 
       title = "Total spatial residual",
       subtitle ="(spatial + non-spatial effects)",
       fill = "Fitted value",
       lty = "", pch = "") #-> map_bym

ggsave(here::here(figdir,"fit","fitted_bym.png"), height = 6, width = 6)

ggmap(blt_lines, 
      base_layer = ggplot(clust)) +
  geom_sf(aes(fill = bym_besag, lty = "Survey cluster"), alpha = 0.5) +
  geom_sf(data = clinics, aes(pch = "TB Clinic")) +
  scale_shape_manual(values = 15) +
  scale_fill_gradient2(
    midpoint = 0, low = "blue", mid = "white", high = "red") +
  # scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(x = "", y = "", 
       title = "Fitted spatial random effect",
       subtitle = "",
       fill = "Fitted value",
       lty = "", pch = "") #-> map_besag

ggsave(here::here(figdir,"fit","fitted_bym_besag.png"), height = 6, width = 6)

################################################################################
################################################################################