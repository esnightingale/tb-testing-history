################################################################################
##                          Setup analysis data                               ##
################################################################################

library(tidyverse)
library(sf)
library(ggmap)

datadir <- "C:/Users/phpuenig/Dropbox/SCALE_Hit-TB_modelling-resources/ModellingData/Pre_PS_Data"

# Source utility functions
list.files(here::here("code","utils"), full.names = TRUE) %>% purrr::walk(source)

#-----------#
# Geography #

# Cluster/HSA/clinic area polygons and clinic locations
clust <- readRDS(here::here("data","clusters.rds")) %>% # Survey clusters
  mutate(clustid = as.numeric(gsub("c","",cluster)))
data("clinics", package="BlantyreTBEpi") # TB clinics
data("hsas", package="BlantyreTBEpi") # Health service areas

# Update CRS type
clust <- st_transform(clust, st_crs(4326))
clinics <- st_transform(clinics, st_crs(4326))
hsas <- st_transform(hsas, st_crs(4326))

# Link clusters to clinics
clust_clinic <- read_csv(here::here("data","PreRandomisationClustData_23May2016.csv")) %>% 
  arrange(close_clin) %>% 
  mutate(clustid = as.numeric(gsub("C","",clid)),
         clinic_id = as.numeric(as.factor(close_clin))) 
clinicarea <- clust %>% 
  full_join(clust_clinic) %>% 
  group_by(clinic_id, close_clin) %>% 
  summarise()

# Extract a base map layer to the extent of cluster polygons
bb <- st_bbox(clust)
e <- c(bb$xmin, bb$ymin, bb$xmax, bb$ymax)
names(e) <- c("left","bottom","right","top")
blt_lines <- get_map(location = e, source = "stamen", maptype = "terrain", zoom = 12)

#------------#
# Population #

# Worldpop Malawi population count raster
pop.path <- here::here("data","mwi_ppp_2019_UNadj.tif")
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
dat_all <- readRDS("C:/Users/phpuenig/Dropbox/SCALE_Hit-TB_modelling-resources/ModellingData/Pre_PS_Data/analysis_data.rds") %>% 
  mutate(agegp2 = factor(cut(s09age, breaks = c(17,24,34,44,54,max(s09age, na.rm=T)))),
         pov_score_scale = -scale(pov_score),
         wealth_quant = as.factor(ntile(pov_score_scale, 6)))

dat <- dat_all %>% 
  filter(incl_extended)

################################################################################