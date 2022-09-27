################################################################################
################################################################################

library(tidyverse)
library(ggmap)
library(sf)
library(raster)
library(BlantyreTBEpi)

theme_set(theme_minimal())

################################################################################

#-----------------------------------------#
# Routine surveillance data and geography #

# Study cluster shapefiles from mlwdata repo
# load("C:/Users/phpuenig/OneDrive - London School of Hygiene and Tropical Medicine/TB/mlwdata/data/scale_72_clusters.rda")
# saveRDS(scale_72_clusters, here::here("data","clusters.rds")) 

clust <- readRDS(here::here("data","clusters.rds")) %>% 
  mutate(clust = as.numeric(gsub("c","",cluster)))
data("hsas", package="BlantyreTBEpi")    # Health service areas
data("clinics", package="BlantyreTBEpi") # TB clinics
data("cnrs", package="BlantyreTBEpi")    # Case notifications per HSA

# Update CRS type
clust <- st_transform(clust, st_crs(4326))
hsas <- st_transform(hsas, st_crs(4326))
clinics <- st_transform(clinics, st_crs(4326))
cnrs <- st_transform(cnrs, st_crs(4326))

# Extract a base map layer to the extent of HSA polygons
bb <- st_bbox(hsas)
e <- c(bb$xmin, bb$ymin, bb$xmax, bb$ymax)
names(e) <- c("left","bottom","right","top")
blt_lines_hsa <- get_map(location = e, source = "stamen", maptype = "terrain", zoom = 12)

# Alternative to the extent of cluster polygons
bb <- st_bbox(clust)
e <- c(bb$xmin, bb$ymin, bb$xmax, bb$ymax)
names(e) <- c("left","bottom","right","top")
blt_lines_clust <- get_map(location = e, source = "stamen", maptype = "terrain", zoom = 12)

# Worldpop Malawi population count raster
pop.path <- here::here("data","mwi_ppp_2020_UNadj.tif")
popMLW <- raster::raster(x = pop.path)

# Crop to same map extent
popBLT <- raster::crop(popMLW, raster::extent(bb))
plot(popBLT)

# Reformat for later plotting
popBLT_spdf <- as(popBLT, "SpatialPixelsDataFrame")
popBLT_df <- as.data.frame(popBLT_spdf)

#------------------------#
# Prevalence survey data #

dat_all <- readRDS("C:/Users/phpuenig/Dropbox/SCALE_Hit-TB_modelling-resources/ModellingData/Pre_PS_Data/indiv_clean.rds") %>% 
  dplyr::select(ind_id, s02cl_id, clust, hh_id, gps_lat, gps_lng, gps_alt, gps_acc, 
                hh_per_room, wealth_quantil, s07sex, sex, s09age, agegp, hiv_combined,
                previous_tb, TB_contact, any_tb_symp, s50tb_medtest, s51tb_spnum, 
                s53tb_evrxray, s54tb_xraynum, s56tb_evrtst, s111serv_hosp, s111serv_hosp_period, d12sumres) %>% 
  mutate(incl_extended = !is.na(s50tb_medtest),
         w_gps = !is.na(gps_lat),
         test_history = (s50tb_medtest == 1 | s53tb_evrxray == 1))

# ---------------------------------------------------------------------------- #
# Describe study region

ggmap(blt_lines_hsa, 
      base_layer = ggplot(hsas)) +
  geom_raster(data = popBLT_df, aes(x, y, fill = mwi_ppp_2020_UNadj), alpha = 0.5) +
  geom_sf(fill = NA, aes(lty = "CHW Area")) +
  geom_sf(data = clust, fill = NA, aes(lty = "Survey cluster")) +
  geom_sf(data = clinics, aes(pch = "TB clinic")) +
  scale_fill_viridis_c(trans = "sqrt", option = "magma", direction = -1, begin = 0.2) +
  scale_shape_manual(values = 15) +
  scale_linetype_manual(values = c("dashed","solid")) +
  labs(x = "", y = "", 
       fill = "Population",
       lty = "", pch = "")
ggsave(here::here("figures","study_region.png"), 
       height = 8, width = 6)

ggmap(blt_lines_clust, 
      base_layer = ggplot(clust)) +
  geom_raster(data = popBLT_df, aes(x, y, fill = mwi_ppp_2020_UNadj), alpha = 0.5) +
  geom_sf(fill = NA, aes(lty = "Study cluster")) +
  geom_sf(data = clinics, aes(pch = "TB clinic")) +
  scale_fill_viridis_c(trans = "sqrt", option = "magma", direction = -1, begin = 0.2) +
  scale_shape_manual(values = 15) +
  labs(x = "", y = "", 
       fill = "Population",
       lty = "", pch = "")

ggsave(here::here("figures","study_region_clusters.png"), 
       height = 8, width = 6)

# ---------------------------------------------------------------------------- #
# Summarise prevalence data

summary(dat_all$s09age)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   18.00   22.00   28.00   32.23   39.00   98.00    5071 
summary(dat_all$sex)
# Female   Male   NA's 
#   9766   6131   5058 

tab_age_sex <- table(dat_all$sex, dat_all$agegp, useNA = "always")
tab_age_sex
#        18-24 25-34 35-44 45-54 55-64  65+ <NA>
# Female  3397  2962  1850   778   455  314   10
# Male    2600  1543   897   492   289  307    3
# <NA>       0     0     0     0     0    0 5058

# 24% missing age/sex. All those missing sex are also missing age,
# but 13 not missing sex are missing age.

vtable::sumtable(dat_all,
                 vars = c("sex","agegp","previous_tb","TB_contact","incl_extended",
                          "test_history","d12sumres",
                          "hh_per_room","wealth_quantil","w_gps"),
                 summ = c('notNA(x)', 'mean(x)', 'sd(x)', 'min(x)','max(x)', "propNA(x)"))

# Tally individuals and households by participation in extended questionnaire
# and presence of household coordinates
dat_all %>%
  summarise(N = n(),
            N_hh = n_distinct(hh_id)) %>% 
  mutate(order = 1,
         incl_extended = NA) %>% 
  pivot_wider() -> all

dat_all %>%
  group_by(incl_extended) %>% 
  summarise(N = n(),
            N_hh = n_distinct(hh_id)) %>% 
  mutate(order = 2) %>% 
  pivot_wider() -> by_ext

dat_all %>%  
  group_by(incl_extended, w_gps) %>% 
  summarise(N = n(),
            N_hh = n_distinct(hh_id)) %>% 
  mutate(order = 3) %>% 
  pivot_wider() -> by_ext_gps

all %>% 
  bind_rows(by_ext) %>% 
  bind_rows(by_ext_gps) %>% 
  arrange(order) %>% 
  dplyr::select(incl_extended, w_gps, N, N_hh) -> tab
tab
#   incl_extended w_gps     N  N_hh
# 1 NA            NA    20955  7225
# 2 FALSE         NA    18217  7038
# 3 TRUE          NA     2738  2278
# 4 FALSE         FALSE    78    43
# 5 FALSE         TRUE  18139  6995
# 6 TRUE          FALSE    29    25
# 7 TRUE          TRUE   2709  2253

write.csv(tab, here::here("output","tab_extended_gps.csv"), row.names = F)

# ~15% individuals given the extended questionnaire including testing history,
# of whom ~99% had household GPS collected

dat_ext <- dat_all %>% 
  filter(!is.na(s50tb_medtest)) %>% 
  mutate(test_history = factor(test_history, levels = c(FALSE,TRUE), labels = c("No","Yes")))

tab_age_sex_ext <- table(dat_ext$sex, dat_ext$agegp, useNA = "always")
tab_age_sex_ext
#        18-24 25-34 35-44 45-54 55-64 65+ <NA>
# Female   563   510   324   140    65  59    3
# Male     465   251   151    90    58  59    0
# <NA>       0     0     0     0     0   0    0

vtable::sumtable(dat_ext,
                 vars = c("hh_per_room","wealth_quantil","gps_lat","gps_lng","sex","agegp",
                          "hiv_combined","previous_tb","TB_contact","test_history",
                          "d12sumres"),
                 summ = c('notNA(x)', 'mean(x)', 'sd(x)', 'min(x)','max(x)', "propNA(x)"))

# ---------------------------------------------------------------------------- #
# Map out geo-located households

dat.sf.hh <- dat_all %>% 
  filter(!is.na(gps_lng)) %>% 
  st_as_sf(coords = c("gps_lng", "gps_lat"), crs = 4326, remove = FALSE)

ggmap(blt_lines_clust, 
      base_layer = ggplot(clust)) +
  geom_sf(fill = "grey", aes(lty = "Survey cluster"), alpha = 0.5) +
  geom_sf(data = clinics, aes(pch = "TB Clinic")) +
  geom_sf(data = dat.sf.hh, 
          aes(col = "Respondent"), cex = 0.8) +
  scale_shape_manual(values = 15) +
  scale_color_manual(values = "indianred") +
  labs(x = "", y = "", 
       lty = "", pch = "", col = "",
       subtitle = "All geo-located survey households",
       caption = paste0("N = ",sum(is.na(dat_all$gps_lng))," with missing household coordinates excluded"))

ggsave(here::here("figures","study_region_all.png"), 
       height = 8, width = 6)

ggmap(blt_lines_clust, 
      base_layer = ggplot(clust)) +
  geom_sf(fill = "grey", aes(lty = "Survey cluster"), alpha = 0.5) +
  geom_sf(data = clinics, aes(pch = "TB Clinic")) +
  geom_sf(data = filter(dat.sf.hh, incl_extended), 
          aes(col = "Respondent"), cex = 0.8) +
  scale_shape_manual(values = 15) +
  scale_color_manual(values = "indianred") +
  labs(x = "", y = "", 
       lty = "", pch = "", col = "",
       subtitle = "Households of extended questionnaire respondents",
       caption = paste0("N = ",sum(is.na(dat_all$gps_lng))," with missing household coordinates excluded"))

ggsave(here::here("figures","study_region_extended.png"), 
       height = 8, width = 6)

################################################################################