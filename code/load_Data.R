################################################################################
## LOAD DATA ##

library(tidyverse)
library(sf)
library(ggmap)

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
prev_clust <- readRDS(here::here("data","dat_scale.rds")) %>% 
  dplyr::select(-popdens) %>% 
  # Calculate average observed incidence over last four years
  mutate(across(starts_with('total_tbcases_'), ~ ./get(str_replace(cur_column(), 
                                                                   'total_tbcases_', 'pop')), 
                .names = 'inc_{.col}')) %>% 
  rowwise() %>% 
  mutate(clust_inc_1518 = 1000*mean(c_across(starts_with("inc_"))))

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
  mutate(pov_score_exp = boot::inv.logit(pov_score),
         pov_quant = ntile(pov_score, 6),
         pov01 = (pov_score > 0.5),
         any_tb_symp = as.factor(any_tb_symp),
         current_trt = factor((s57tb_treat == 1), 
                              levels = c(FALSE,TRUE), 
                              labels = c("No","Yes")),
         incl_extended = !is.na(s50tb_medtest),
         test_history = factor((s50tb_medtest == 1 | s53tb_evrxray == 1), 
                               levels = c(FALSE,TRUE), 
                               labels = c("No","Yes"))) %>% 
  rename(wealth_step = h22hh_step) %>% 
  # Count participants per cluster
  group_by(s02cl_id) %>% 
  mutate(clust_N_tot = n(),
         clust_N_ext = sum(incl_extended)) %>% 
  ungroup() 


################################################################################