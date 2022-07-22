################################################################################
################################################################################

library(tidyverse)
library(ggmap)
library(sf)
library(raster)
library(BlantyreTBEpi)

theme_set(theme_minimal())

figdir <- "treatment history/figures"

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
                s53tb_evrxray, s54tb_xraynum, s56tb_evrtst, s111serv_hosp, s111serv_hosp_period) %>% 
  mutate(incl_extended = !is.na(s50tb_medtest),
         w_gps = !is.na(gps_lat),
         test_history = (s50tb_medtest == 1 | s53tb_evrxray == 1))

# Extract extended questionnaire obs
dat_ext <- dat_all %>% 
  filter(!is.na(s50tb_medtest)) %>% 
  mutate(test_history = factor(test_history, levels = c(FALSE,TRUE), labels = c("No","Yes")))

# ---------------------------------------------------------------------------- #
# Summarise prevalence data

summary(dat_all)

dat_ext %>% 
  group_by(s50tb_medtest, s53tb_evrxray) %>% 
  tally()
#   s50tb_medtest s53tb_evrxray     n
#       <dbl+lbl>     <dbl+lbl> <int>
# 1       1 [Yes]       1 [Yes]   114
# 2       1 [Yes]       2 [No]    161
# 3       2 [No]        1 [Yes]   158
# 4       2 [No]        2 [No]   2305

summary(as.factor(dat_ext$s50tb_medtest))
#   1    2 
# 275 2463
summary(as.factor(dat_ext$s53tb_evrxray))
#   1    2 
# 272 2466 
summary(dat_ext$test_history)
#   No  Yes 
# 2305  433 

# Tally individuals and households by participation in extended questionnaire
# and presence of household coordinates
dat_all %>%
  summarise(N = n_distinct(ind_id),
            N_hh = n_distinct(hh_id)) %>% 
  mutate(order = 1,
         incl_extended = NA) %>% 
  pivot_wider() -> all

dat_all %>%
  group_by(incl_extended) %>% 
  summarise(N = n_distinct(ind_id),
            N_hh = n_distinct(hh_id)) %>% 
  mutate(order = 2) %>% 
  pivot_wider() -> by_ext

dat_all %>%  
  group_by(incl_extended, w_gps) %>% 
  summarise(N = n_distinct(ind_id),
            N_hh = n_distinct(hh_id)) %>% 
  mutate(order = 3) %>% 
  pivot_wider() -> by_ext_gps

all %>% 
  bind_rows(by_ext) %>% 
  bind_rows(by_ext_gps) %>% 
  arrange(order) %>% 
  dplyr::select(incl_extended, w_gps, N, N_hh) -> tab
write.csv(tab, here::here("output","tab1.csv"), row.names = F)

# ~15% individuals given the extended questionnaire including testing history,
# of whom ~99% had household GPS collected

vtable::sumtable(dat_ext,
                 vars = c("hh_per_room","wealth_quantil","gps_lat","gps_lng","sex","agegp",
                          "hiv_combined","previous_tb","TB_contact","test_history",
                          "d12sumres"),
                 summ = c('notNA(x)', 'mean(x)', 'sd(x)', 'min(x)','max(x)', "propNA(x)"))


# ---------------------------------------------------------------------------- #
# Summarise test history by cluster

dat_ext %>% 
  group_by(clust) %>% 
  summarise(n = n(),
            x = sum(test_history == "Yes"),
            perc_test_history = 100*Hmisc::binconf(x, n)[1],
            ci_low = 100*Hmisc::binconf(x, n)[2],
            ci_high = 100*Hmisc::binconf(x, n)[3]) -> by_clust

summary(by_clust)
# clust           n               x                prop_test_history ci_low            ci_high      
# Min.   : 1.00   Min.   :14.00   Min.   : 0.000   Min.   :0.0000    Min.   :0.00000   Min.   :0.1483  
# 1st Qu.:18.75   1st Qu.:32.00   1st Qu.: 3.000   1st Qu.:0.1133    1st Qu.:0.04259   1st Qu.:0.2460  
# Median :36.50   Median :39.50   Median : 6.000   Median :0.1508    Median :0.06697   Median :0.3059  
# Mean   :36.50   Mean   :38.03   Mean   : 6.014   Mean   :0.1510    Mean   :0.07348   Mean   :0.3002  
# 3rd Qu.:54.25   3rd Qu.:47.00   3rd Qu.: 9.000   3rd Qu.:0.2054    3rd Qu.:0.11291   3rd Qu.:0.3554  
# Max.   :72.00   Max.   :63.00   Max.   :13.000   Max.   :0.2750    Max.   :0.16211   Max.   :0.4815  

# Overall mean + CI:
overall <- 100*round(Hmisc::binconf(x = sum(dat_ext$test_history == "Yes"), n = nrow(dat_ext)),2)
# PointEst Lower Upper
#       16    14    17

ggplot(by_clust, aes(x = perc_test_history)) +
  geom_histogram(binwidth = 2) +
  geom_vline(aes(xintercept = mean(perc_test_history)), lty = "dashed") +
  labs(title = "Percentage of respondents reporting previous sputum test or chest x-ray, per cluster",
       # subtitle = paste0("Mean = ", round(mean(by_clust$prop_test_history),2)),
       x = "",
       y = "Frequency")
ggsave(here::here(figdir,"hist_perc_history_byclust.png"), 
       height = 6, width = 8)


ggplot(by_clust, aes(x = as.factor(clust), y = prop_test_history, ymin = ci_low, ymax = ci_high)) +
  geom_point() +
  geom_errorbar(width = 0.2) + 
  labs(title = "Percentage (with 95% CI) of respondents reporting previous sputum test or chest x-ray, by cluster",
       subtitle = paste0("Overall mean = ", overall[1],
                         " [", overall[2],"-", overall[3],"]"),
       x = "Survey cluster",
       y = "")
ggsave(here::here(figdir,"perc_history_byclust.png"), 
       height = 6, width = 15)

# ---------------------------------------------------------------------------- #
# Map out clusters

dat_ext %>% 
  group_by(clust) %>% 
  summarise(N = n(),
            N_test_hist = sum(test_history == "Yes")) %>% 
  mutate(perc_test_hist = N_test_hist*100/N) -> dat.clust

clust %>% 
  full_join(dat.clust) -> dat.clust

ggmap(blt_lines_clust, 
      base_layer = ggplot(dat.clust)) +
  geom_sf(aes(fill = perc_test_hist, lty = "Survey cluster"), alpha = 0.5) +
  geom_sf(data = clinics, aes(pch = "TB Clinic")) +
  scale_shape_manual(values = 15) +
  scale_fill_viridis_c(option = "inferno", direction = -1) +
  labs(x = "", y = "", 
       fill = "%",
       lty = "", pch = "", 
       subtitle = "% respondents reporting previous test or x-ray by cluster") 

ggsave(here::here(figdir,"test_xray_history_byclust.png"), 
       height = 5, width = 6)

# ---------------------------------------------------------------------------- #
# By HSA

sf::sf_use_s2(FALSE)

dat.sf.hh %>% 
  st_join(hsas, join = st_within) %>% 
  st_drop_geometry() %>% 
  group_by(c02hsaid) %>% 
  summarise(N = n(),
            N_test_hist = sum(test_history == "Yes")) %>% 
  mutate(prop_test_hist = N_test_hist*100/N) -> dat.hsa

hsas %>% 
  full_join(dat.hsa) -> dat.hsa

ggmap(blt_lines_hsa, 
      base_layer = ggplot(dat.hsa)) +
  geom_sf(aes(fill = prop_test_hist, lty = "CHW area"), alpha = 0.5) +
  geom_sf(data = clinics, aes(pch = "TB Clinic")) +
  scale_shape_manual(values = 15) +
  scale_fill_viridis_c(option = "inferno", direction = -1) +
  labs(x = "", y = "", 
       fill = "% respondents",
       lty = "", pch = "", 
       subtitle = "% respondents reporting previous test or x-ray by CHW area",
       caption = "N = 29 with missing household coordinates excluded") 

ggsave(here::here(figdir,"test_xray_history_byHSA.png"), 
       height = 8, width = 6)

# ---------------------------------------------------------------------------- #
# Standardised incidence ratios

dat_ext %>%  
  summarise(N = n(),
            x = sum(test_history == "Yes")) %>% 
  ungroup() %>% 
  mutate(rate_E = x/N)-> total

# Totals per cluster:
dat_ext %>% 
  group_by(clust) %>% 
  summarise(N = n(),
            x = sum(test_history == "Yes")) %>% 
  mutate(rate_obs = x/N) -> byclust

byclust %>% 
  mutate(E = N*total$rate_E) -> byclust

byclust %>% 
  group_by(clust) %>% 
  summarise(N = sum(N),
            E = sum(E, na.rm = T),
            O = sum(x),
            E_rate = E/N,
            O_rate = O/N) %>% 
  ungroup() %>% 
  mutate(clust = as.numeric(as.factor(clust)),
         SIR = O/E)-> total_E_byclust

clust %>% 
  full_join(total_E_byclust) -> dat.E.clust

ggmap(blt_lines_clust, 
      base_layer = ggplot(dat.E.clust)) +
  geom_sf(aes(fill = SIR, lty = "Survey cluster"), alpha = 0.5) +
  geom_sf(data = clinics, aes(pch = "TB Clinic")) +
  scale_shape_manual(values = 15) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red"
  ) +
  # scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(x = "", y = "", 
       fill = "SIR",
       lty = "", pch = "", 
       title = "Standardised incidence ratios of TB testing history",
       subtitle = "per cluster")

ggsave(here::here(figdir,"testhist_SIR_byclust.png"),
       height = 5, width = 6)

# ---------------------------------------------------------------------------- #
# Age/sex-standardised

# Calculate age and sex-adjusted expected counts for each neighbourhood, 
# based on overall totals

# Total respondents reporting testing history by age group and sex:
dat_ext %>% 
  group_by(sex, agegp) %>% 
  summarise(N = n(),
            x = sum(test_history == "Yes")) %>% 
  ungroup() %>% 
  mutate(rate_E = x/N)-> age_sex_totals

# Totals per cluster:
dat_ext %>% 
  group_by(clust, sex, agegp) %>% 
  summarise(N = n(),
            x = sum(test_history == "Yes")) %>% 
  mutate(rate_obs = x/N) -> age_sex_byclust

age_sex_byclust %>% 
  full_join(dplyr::select(age_sex_totals, sex, agegp, rate_E)) %>% 
  mutate(E = N*rate_E) -> age_sex_byclust

age_sex_byclust %>% 
  group_by(clust) %>% 
  summarise(N = sum(N),
            E = sum(E),
            O = sum(x),
            E_rate = E/N,
            O_rate = O/N) %>% 
  ungroup() %>% 
  mutate(clust = as.numeric(as.factor(clust)),
         SIR = O/E)-> total_E_byclust

clust %>% 
  full_join(total_E_byclust) -> dat.E.clust

ggmap(blt_lines_clust, 
      base_layer = ggplot(dat.E.clust)) +
  geom_sf(aes(fill = SIR, lty = "Survey cluster"), alpha = 0.5) +
  geom_sf(data = clinics, aes(pch = "TB Clinic")) +
  scale_shape_manual(values = 15) +
  scale_fill_gradient2(
    midpoint = 1, low = "blue", mid = "white", high = "red"
  ) +
  # scale_fill_viridis_c(option = "plasma", direction = -1) +
  labs(x = "", y = "", 
       fill = "SIR",
       lty = "", pch = "", 
       title = "Age-sex standardised incidence rates of TB testing history",
       subtitle = "per cluster") 

ggsave(here::here(figdir,"testhist_agesex_SIR_byclust.png"), 
       height = 5, width = 6)

################################################################################
