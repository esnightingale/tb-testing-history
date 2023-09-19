################################################################################
# Descriptive analysis of self-reported TB testing history, by cluster, with
# respect to sex, age, HIV status and two alternate measures of poverty. 
# 
# 1. Totals reporting previous testing
# 2. Tabulate by covariates of interest
# 3. Map by survey cluster and by associated clinic
# 4. Summarise/compare poverty measures, map by cluster
#
################################################################################

library(tidyverse)
library(lubridate)
library(ggmap)
library(ggspatial)
library(sf)
library(patchwork)
library(spdep)

# Fix default ggsave background
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

theme_set(theme_minimal())

figdir <- "figures/descriptive"

# Source data loading script
source(here::here("code/load_data.R"), echo=TRUE)

# Source function for table
source(here::here("code/utils/make_tab.R"), echo=TRUE)

dat_nona <- dat %>% 
  filter(!is.na(s09age) & hiv_combined != "HIV unknown" & !is.na(pov_score))

################################################################################

# Describe study region 

ggmap(blt_lines, 
      base_layer = ggplot(clust)) +
  geom_raster(data = popBLT_df, aes(x, y, fill = mwi_ppp_2019_UNadj), alpha = 0.5) +
  geom_sf(data = clust, fill = NA, aes(lty = "Survey cluster")) +
  geom_sf(data = clinics, aes(pch = "TB clinic")) +
  scale_fill_viridis_c(trans = "sqrt", option = "magma", direction = -1, begin = 0.2) +
  scale_shape_manual(values = 15) +
  labs(x = "", y = "", 
       caption = NULL,
       fill = expression(Population~per~"100m"^2),
       lty = "", pch = "") +
  annotation_scale(location = "br") +
  annotation_north_arrow(location = "bl",
                         style = north_arrow_minimal(),
                         height = unit(0.8, "cm"), width = unit(0.8, "cm")) +
  theme(axis.text = element_blank(),
        text = element_text(size = 16)) 

ggsave(here::here(figdir,"study_region.png"), 
       height = 5, width = 7, dpi = 500)

ggsave(here::here("figures/manuscript","fig1.png"), 
       height = 5, width = 7, dpi = 500)

# ---------------------------------------------------------------------------- #
# Household composition

# Household size over full dataset? Not only extended survey?

dat %>% 
  group_by(hh_id) %>% 
  summarise(n = n(),
            hh_per_room = median(hh_per_room)) %>%
  ungroup() -> byhh

byhh %>% 
  dplyr::select(-hh_id) %>% 
  summary()

#        n          hh_per_room   
# Min.   :1.000   Min.   :0.200  
# 1st Qu.:1.000   1st Qu.:1.000  
# Median :1.000   Median :1.333  
# Mean   :1.202   Mean   :1.417  
# 3rd Qu.:1.000   3rd Qu.:1.667  
# Max.   :5.000   Max.   :5.000  
#                 NA's   :25  

summary(as.factor(byhh$n))
#    1    2    3    4    5 
# 1885  334   52    6    1 

# ---------------------------------------------------------------------------- #
# Overall totals - including missing data

dat %>% 
  mutate(sputum = (s50tb_medtest == 1),
         xray = (s53tb_evrxray == 1)) %>% 
  group_by(sputum, xray) %>% 
  count() 
#   sputum xray      n
# 1 FALSE  FALSE  2305
# 2 FALSE  TRUE    158
# 3 TRUE   FALSE   161
# 4 TRUE   TRUE    114

n_distinct(dat$hh_id) # 2278

dat %>% 
  group_by(last_test_12m) %>% 
  count()
#   last_test_12m     n
# 1 No             2627
# 2 Yes             111

mean(dat$s50tb_medtest == 1) #0.1004383
mean(dat$s53tb_evrxray == 1) #0.09934259
mean(dat$s50tb_medtest == 1 & dat$s53tb_evrxray == 1) #0.04163623

# ---------------------------------------------------------------------------- #
# Overall totals - excluding missing data

sum(dat_nona$ever_test == "Yes")
# 414
sum(dat_nona$s50tb_medtest == 1)
# 257
sum(dat_nona$s53tb_evrxray == 1)
# 262

dat_nona %>% 
  mutate(sputum = (s50tb_medtest == 1),
         xray = (s53tb_evrxray == 1)) %>% 
  group_by(sputum, xray) %>% 
  count() 
#   sputum xray      n
# 1 FALSE  FALSE  2176
# 2 FALSE  TRUE    157
# 3 TRUE   FALSE   152
# 4 TRUE   TRUE    105

n_distinct(dat_nona$hh_id) # 2167

dat_nona %>% 
  group_by(last_test_12m) %>% 
  count()
#   last_test_12m     n
# 1 No             2484
# 2 Yes             106

mean(dat_nona$s50tb_medtest == 1) #0.099
mean(dat_nona$s53tb_evrxray == 1) #0.101
mean(dat_nona$s50tb_medtest == 1 & dat_nona$s53tb_evrxray == 1) #0.0405

Hmisc::binconf(sum(dat_nona$ever_test == "Yes"), nrow(dat_nona))
# PointEst     Lower     Upper
# 0.1598456 0.1462375 0.1744611

# ---------------------------------------------------------------------------- #
# Last test dates

dat_nona %>% 
  filter(ever_test == "Yes") %>% 
  mutate(time_since_test = time_length(difftime(today, last_test), "months")) -> tested 
  
summary(tested$last_test)
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max.         NA's 
# "1995-05-01" "2018-01-01" "2019-03-01" "2017-07-07" "2019-09-01" "2019-12-01"        "248" 

summary(tested$time_since_test)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#  -0.4928   2.4394   6.9487  27.7345  23.3347 289.6099      248 

mean(is.na(tested$last_test))
# 0.5990338

ggplot(tested, aes(last_test)) +
  geom_histogram() +
  labs(x = "Reported date of last test", y = "Frequency")

tested %>% 
  mutate(last_test_m = floor_date(last_test, "month")) %>% 
  group_by(last_test_m) %>% 
  count() %>% 
  ggplot(aes(last_test_m, n)) +
  geom_col()

summary(tested$last_test > as.Date("01/01/2016"))
#    Mode    TRUE    NA's 
# logical     166     248 

# Make table
tested %>% 
  pivot_longer(c("last_sputum","last_xray")) %>% 
  mutate(time_since_test = time_length(difftime(today, value), "months")) -> tmp

tested %>% 
  mutate(name = "overall",
         value = last_test) %>% 
  bind_rows(tmp) %>%  
  group_by(name) %>% 
  summarise(nonmissing = sum(!is.na(value)),
            min = min(value, na.rm = T),
            median = median(value, na.rm = T),
            max = max(value, na.rm = T),
            # diff = time_length(difftime(max, min), "years"),
            time_since_test_med = round(median(time_since_test, na.rm = T), 2),
            time_since_test_iqr = paste(round(quantile(time_since_test,
                                                       c(0.25,0.75),
                                                       na.rm = T),
                                              2), 
                                        collapse = "-"),
            tested_12m_n = sum(!is.na(value) & time_since_test < 12),
            tested_12m_n_p = paste0(tested_12m_n, " (",
                                   round(tested_12m_n*100/nonmissing,1), 
                                   ")")) -> tab_lasttest

tab_lasttest

# Save table
write_csv(tab_lasttest, here::here("output/descriptive","tabS1_last_test.csv"))

# ---------------------------------------------------------------------------- #
# Sample age-sex distribution

dat %>%
  filter(!is.na(agegp)) %>% 
  group_by(agegp) %>% 
  summarise(Male = sum(sex == "Male"),
            Female = sum(sex == "Female")) %>% 
  pivot_longer(-agegp) %>% 
  mutate(value = ifelse(name == "Male", value*(-1), value*1)) %>%
  ggplot(aes(x = agegp, y = value, fill=name)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(fill = "", y = "Frequency", x = "Age group")

ggsave(here::here(figdir, "sample_age_sex_pyramid.png"), height = 5, width = 6, dpi = 500)

# Continuous age
dat %>% 
  ggplot(aes(s09age, as.numeric(ever_test)-1)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  labs(x = "Age (years)", y = "Ever tested")

dat %>% 
  ggplot(aes(s09age, as.numeric(ever_test)-1, col = sex, fill = sex)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  guides(fill = "none") +
  labs(x = "Age (years)", y = "Ever tested", col = "Sex") +
  theme(text = element_text(size = 14))

ggsave(here::here("figures/manuscript/supplementary", "figs4.png"), height = 5, width = 6)

# Grouped age
pos <- position_dodge(0.5)
dat %>% 
  filter(!is.na(s09age)) %>% 
  mutate(agegp = factor(cut(s09age, breaks = c(17,24,34,44,54,max(s09age, na.rm=T))))) %>% 
  group_by(sex, agegp) %>% 
  summarise(n = n(),
            x = sum(ever_test == "Yes"),
            p_test = x/n,
            low = Hmisc::binconf(x, n)[2],
            hi = Hmisc::binconf(x, n)[3]) %>% 
  ungroup() %>% 
  ggplot(aes(x = agegp, y = p_test, ymin = low, ymax = hi, col = sex)) +
  geom_errorbar(position= pos, width = 0.3) +
  geom_point(position = pos) +
  labs(x = "Age group", y = "% Ever test", col = "", caption = "95% binomial confidence intervals") +
  theme(text = element_text(size = 14))

ggsave(here::here(figdir,"testhist_by_agegp_sex.png"), height = 4, width = 6)

dat$agegp <- factor(cut(dat$s09age, breaks = c(17,24,34,44,54,max(dat$s09age, na.rm=T))))

# ---------------------------------------------------------------------------- #
# Summarise by individual characteristics (Table 1)

fact_vars <- list(sex = dat$sex,
                  agegp = dat$agegp,
                  hiv_ever = dat$hiv_ever,
                  hiv = dat$hiv_combined,
                  wealth_quant = dat$wealth_quant,
                  wealth_step = dat$wealth_step,
                  anyone_tb = dat$s60tb_anyone, 
                  fam_tb = dat$s62tb_famtb, 
                  died_tb = dat$s65tb_died)

tab1a <- make_tab(fact_vars, Var2 = dat$ever_test) %>%
  rename(ever_test = summ_yes,
         ever_test_p = p)
tab1b <- make_tab(fact_vars, Var2 = dat$last_test_12m) %>%
  rename(last_test_12m = summ_yes,
         last_test_12m_p = p)

tab1 <- tab1a %>% 
  full_join(tab1b)
write.csv(tab1, here::here("output", "tab1.csv"), row.names = FALSE)

#------------------------------------------------------------------------------#
# Unknown HIV status

dat$hivmiss <- factor(dat$hiv_combined == "HIV unknown", levels = c(FALSE,TRUE), labels = c("No","Yes"))

# Tabulate
fact_vars2 <- list(sex = dat$sex,
                   agegp = dat$agegp,
                   wealth_quant = dat$wealth_quant,
                   wealth_step = dat$wealth_step)
tab2 <- make_tab(fact_vars2, Var2 = dat$hivmiss)
write.csv(tab2, here::here("output", "tab2_hivmissing.csv"), row.names = FALSE)

# Plot by vars
dat %>% 
  ggplot(aes(x = hivmiss, y = s09age)) +
  geom_boxplot() +
  labs(x = "Unknown HIV status", y = "Age (years)") -> byage

dat %>% 
  ggplot(aes(x = hivmiss, y = pov_score)) +
  geom_boxplot() +
  labs(x = "Unknown HIV status", y = "PMT score") -> bypmt

dat %>% 
  ggplot(aes(x = hivmiss, y = as.numeric(wealth_step))) +
  geom_boxplot() +
  labs(x = "Unknown HIV status", y = "Wealth step") -> bystep

byage + bypmt + bystep
ggsave(here::here(figdir,"missing_hiv_by_age_pov.png"), height = 5, width = 15)

plot_bars_byperc(dat, "wealth_quant","hivmiss") +
  labs(x = "Poverty score quantile", y = "%", fill = "HIV unknown") +
  guides(fill = "none") -> bars_bypmt
plot_bars_byperc(dat, "wealth_step","hivmiss") +
  labs(x = "Self-assessed wealth", y = "%", fill = "HIV unknown") -> bars_bystep

bars_bypmt + bars_bystep
ggsave(here::here(figdir,"missing_hiv_by_pov.png"), height = 5, width = 10)

# Plot with 95% CIs
vars <- c(Sex = "sex",`Age group` = "agegp",`Self-assessed wealth` = "wealth_step",`Wealth quantile` = "wealth_quant")
p.list <- purrr::map2(vars, names(vars), function(x, nm) plot_ci(dat, x, "hivmiss") + labs(x = nm))
p.grid <- gridExtra::grid.arrange(grobs = p.list)

ggsave(here::here(figdir,"missing_hiv_byvars.png"), p.grid, height = 10, width = 10)


# Plot by areas
dat %>% 
  group_by(clustid) %>% 
  summarise(n = n(),
            p_hivmiss = 100*sum(hivmiss=="Yes")/n,
            ci_low = 100*Hmisc::binconf(sum(hivmiss=="Yes"), n)[2],
            ci_high = 100*Hmisc::binconf(sum(hivmiss=="Yes"), n)[3]) -> hivmiss_byclust

ggplot(hivmiss_byclust, aes(x = as.factor(clustid), y = p_hivmiss, ymin = ci_low, ymax = ci_high)) +
  geom_hline(yintercept = 100*mean(dat$hivmiss == "Yes"), col = "red", lty = "dashed") +
  geom_point() +
  geom_errorbar(width = 0.2) + 
  labs(title = "Percentage (with 95% CI) of respondents with unknown HIV status, by cluster",
       x = "Survey cluster",
       y = "",
       caption = "Overall percentage shown in red.") -> ci_byclust

ci_byclust
ggsave(here::here(figdir,"perc_hivmissing_byclust.png"), 
       height = 5, width = 15)


plot_byarea(hivmiss_byclust, clust, "p_hivmiss", lab = "% HIV\nunknown") +
  labs(caption = "") -> byclust

dat %>% 
  group_by(clinic_id) %>% 
  summarise(n = n(),
            p_hivmiss = 100*sum(hivmiss=="Yes")/n,
            ci_low = 100*Hmisc::binconf(sum(hivmiss=="Yes"), n)[2],
            ci_high = 100*Hmisc::binconf(sum(hivmiss=="Yes"), n)[3]) -> hivmiss_byclinic

ggplot(hivmiss_byclinic, aes(x = as.factor(clinic_id), y = p_hivmiss, ymin = ci_low, ymax = ci_high)) +
  geom_hline(yintercept = 100*mean(dat$hivmiss == "Yes"), col = "red", lty = "dashed") +
  geom_point() +
  geom_errorbar(width = 0.2) + 
  labs(title = "Percentage (with 95% CI) of respondents with unknown HIV status, by clinic area",
       x = "Clinic area",
       y = "",
       caption = "Overall percentage shown in red.") -> ci_byclinic

ci_byclinic
ggsave(here::here(figdir,"perc_hivmiss_byclinic.png"), 
       height = 5, width = 15)

plot_byarea(hivmiss_byclinic, clinicarea, "p_hivmiss", lab = "% HIV\nunknown") -> byclinic

byclust + byclinic
ggsave(here::here(figdir,"missing_hiv_by_area.png"), height = 5, width = 12)

# ---------------------------------------------------------------------------- #
# Unknown household poverty

dat$povmiss <- factor(is.na(dat$pov_score), levels = c(FALSE,TRUE), labels = c("No","Yes"))

# Tabulate
fact_vars3 <- list(sex = dat$sex,
                   agegp = dat$agegp,
                   hiv = dat$hiv_combined)
tab3 <- make_tab(fact_vars3, Var2 = dat$povmiss)
write.csv(tab3, here::here("output", "tab2_povmissing.csv"), row.names = FALSE)

# Plot by vars
dat %>% 
  ggplot(aes(x = povmiss, y = s09age)) +
  geom_boxplot() +
  labs(x = "Missing household wealth data", y = "Age (years)") -> byage

byage
ggsave(here::here(figdir,"missing_pov_by_age.png"), height = 5, width = 5)

# Plot by areas
dat %>% 
  group_by(clustid) %>% 
  summarise(n = n(),
            x = sum(povmiss=="Yes"),
            p_povmiss = 100*x/n,
            ci_low = 100*Hmisc::binconf(x, n)[2],
            ci_high = 100*Hmisc::binconf(x, n)[3]) -> povmiss_byclust

ggplot(povmiss_byclust, aes(x = as.factor(clustid), y = p_povmiss, ymin = ci_low, ymax = ci_high)) +
  geom_hline(yintercept = 100*mean(dat$povmiss == "Yes"), col = "red", lty = "dashed") +
  geom_point() +
  geom_errorbar(width = 0.2) + 
  labs(title = "Percentage (with 95% CI) of respondents with missing household wealth data, by cluster",
       x = "Survey cluster",
       y = "",
       caption = "Overall percentage shown in red.") -> ci_byclust

ci_byclust
ggsave(here::here(figdir,"perc_povmissing_byclust.png"), 
       height = 5, width = 15)


plot_byarea(povmiss_byclust, clust, "p_povmiss", lab = "% missing") +
  labs(caption = "") -> byclust

dat %>% 
  group_by(clinic_id) %>% 
  summarise(n = n(),
            x = sum(povmiss=="Yes"),
            p_povmiss = 100*x/n,
            ci_low = 100*Hmisc::binconf(x, n)[2],
            ci_high = 100*Hmisc::binconf(x, n)[3]) -> povmiss_byclinic

ggplot(povmiss_byclinic, aes(x = as.factor(clinic_id), y = p_povmiss, ymin = ci_low, ymax = ci_high)) +
  geom_hline(yintercept = 100*mean(dat$povmiss == "Yes"), col = "red", lty = "dashed") +
  geom_point() +
  geom_errorbar(width = 0.2) + 
  labs(title = "Percentage (with 95% CI) of respondents with missing household wealth data, by clinic area",
       x = "Clinic area",
       y = "",
       caption = "Overall percentage shown in red.") -> ci_byclinic

ci_byclinic
ggsave(here::here(figdir,"perc_povmissing_byclinic.png"), 
       height = 5, width = 15)

plot_byarea(povmiss_byclinic, clinicarea, "p_povmiss", lab = "% missing") -> byclinic

byclust + byclinic
ggsave(here::here(figdir,"missing_pov_by_area.png"), height = 5, width = 12)

# ---------------------------------------------------------------------------- #
# Summarise outcome by cluster

dat %>% 
  group_by(clustid) %>% 
  summarise(n = n(),
            x = sum(ever_test == "Yes"),
            x1 = sum(s50tb_medtest == 1),
            x2 = sum(s53tb_evrxray == 1),
            perc_test_history = 100*x/n,
            perc_sputum = 100*x1/n,
            perc_xray = 100*x2/n,
            ci_low = 100*Hmisc::binconf(x, n)[2],
            ci_high = 100*Hmisc::binconf(x, n)[3]) -> by_clust

# Overall mean + CI:
overall <- 100*round(Hmisc::binconf(x = sum(dat$ever_test == "Yes"), n = nrow(dat)),2)
overall
# PointEst Lower Upper
#       16    14    17

100*round(Hmisc::binconf(x = sum(dat$last_test_12m == "Yes"), n = nrow(dat)),2)
# PointEst Lower Upper
#        4     3     5

# Plot cluster summary

ggplot(by_clust, aes(x = as.factor(clustid), y = perc_test_history, ymin = ci_low, ymax = ci_high)) +
  geom_point() +
  geom_errorbar(width = 0.2) + 
  labs(title = "Percentage (with 95% CI) of respondents reporting previous sputum test or chest x-ray, by cluster",
       subtitle = paste0("Overall mean = ", overall[1],
                         " [", overall[2],"-", overall[3],"]"),
       x = "Survey cluster",
       y = "")
ggsave(here::here(figdir,"perc_history_byclust.png"), 
       height = 5, width = 15)

dat_clust_long <- by_clust %>% 
  pivot_longer(perc_test_history:perc_xray) %>% 
  mutate(name = factor(name, 
                       levels = c("perc_test_history","perc_sputum", "perc_xray"),
                       labels = c("Any test","Sputum","CXR"))) 
dat_clust_long %>% 
  ggplot(aes(x = name, y = value)) +
  geom_boxplot() + 
  labs(x = "",y = "Cluster percentage") +
  theme(text = element_text(size = 16)) -> boxplot_testtype
boxplot_testtype
ggsave(here::here(figdir,"box_test_prev_byclust.png"), 
       boxplot_testtype,
       height = 5, width = 5)

# ---------------------------------------------------------------------------- #
# Map out clusters

plot_byarea(by_clust, clust, varname = "perc_test_history", lab = "%") +
  # geom_sf(data = clinics, aes(pch = "TB Clinic")) +
  scale_shape_manual(values = 15) +
  scale_fill_viridis_c(option = "inferno", direction = -1) +
  # scale_fill_gradient2(midpoint = mean(by_clust$perc_test_history), low = "red", high = "blue") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), 
        text = element_text(size = 16)) +
  labs(x = "", y = "", 
       fill = "%",
       lty = "", pch = "", 
       subtitle = "% respondents reporting previous sputum test or CXR",
       caption = NULL) -> test_byclust

test_byclust
ggsave(here::here(figdir,"test_history_byclust.png"), 
       test_byclust,
       height = 5, width = 6)

test_byclust + boxplot_testtype + plot_annotation(tag_levels = "A") +
  plot_layout(widths = c(1,1.1)) -> fig2
fig2
ggsave(here::here("figures/manuscript","fig2.png"), 
       height = 5, width = 12, dpi = 500)
ggsave(here::here("figures/manuscript","fig2.pdf"), 
       height = 5, width = 12, dpi = 500)

by_clust %>% 
  pivot_longer(perc_sputum:perc_xray) %>% 
  mutate(name = factor(name,
                       levels = c("perc_sputum","perc_xray"),
                       labels = c("Sputum","CXR"))) -> by_clust_long

plot_byarea(by_clust_long, clust, varname = "value", lab = "%") +
  # geom_sf(data = clinics, aes(pch = "TB Clinic")) +
  # scale_shape_manual(values = 15) +
  # scale_fill_viridis_c(option = "inferno", direction = -1) +
  labs(x = "", y = "", 
       fill = "%",
       lty = "", pch = "",
       subtitle = "% respondents reporting previous test or chest x-ray, by cluster") +
  facet_wrap(~name)
ggsave(here::here(figdir,"test_history_byclust_type.png"), height = 5, width = 10)

dat %>% 
  group_by(clinic_id) %>% 
  summarise(n = n(),
            x = sum(ever_test == "Yes"),
            x1 = sum(s50tb_medtest == 1),
            x2 = sum(s53tb_evrxray == 1),
            perc_test_history = 100*x/n,
            perc_sputum = 100*x1/n,
            perc_xray = 100*x2/n) %>% 
  pivot_longer(perc_sputum:perc_xray) %>% 
  mutate(name = factor(name,
                       levels = c("perc_sputum","perc_xray"),
                       labels = c("Sputum","CXR"))) -> by_clinic_long

plot_byarea(by_clinic_long, clinicarea, varname = "value", lab = "%") +
  # geom_sf(data = clinics, aes(pch = "TB Clinic")) +
  # scale_shape_manual(values = 15) +
  # scale_fill_viridis_c(option = "inferno", direction = -1) +
  labs(x = "", y = "", 
       fill = "%",
       lty = "", pch = "",
       # caption = NULL,
       subtitle = "% respondents reporting previous test or chest x-ray, by clinic") +
  facet_wrap(~name)
ggsave(here::here(figdir,"test_history_byclinic_type.png"), height = 5, width = 10)

#------------------------------------------------------------------------------#
# Spatial autocorrelation by cluster

# Specify cluster neighbour structure/weights
nb <- poly2nb(clust, snap = 0.001)

# Cluster 31 has no neighbours - impute as 27 for completeness
nb[[31]] = as.integer(27,28)

lw <- nb2listw(nb, style="B", zero.policy=TRUE)
M <- nb2mat(nb, style = "B", zero.policy = TRUE)

# Cluster % tested
MC <- moran.mc(by_clust$perc_test_history, lw, nsim=999)
MC
# statistic = -0.11292, observed rank = 118, p-value = 0.882

plot(MC, main="", las=1, xlab = "Cluster % ever tested")

# By test type
MC_sp <- moran.mc(by_clust$perc_sputum, lw, nsim=999)
MC_sp
# statistic = -0.16568, observed rank = 49, p-value = 0.951


MC_xr <- moran.mc(by_clust$perc_xray, lw, nsim=999)
MC_xr
# statistic = 0.081059, observed rank = 844, p-value = 0.156

png(here::here(figdir, "moran_testtype_byclust.png"), height = 400, width = 800)
par(mfrow = c(1,2))
plot(MC_sp, main="", las=1, xlab = "Cluster % sputum")
plot(MC_xr, main="", las=1, xlab = "Cluster % chest x-ray")
dev.off()

# ---------------------------------------------------------------------------- #
# Map by clinic

dat %>% 
  left_join(clust_clinic, by = c("s02cl_id" = "clustid")) %>% 
  group_by(clinic_id) %>% 
  summarise(n = n(),
            x = sum(ever_test == "Yes"),
            x1 = sum(s50tb_medtest == 1),
            x2 = sum(s53tb_evrxray == 1),
            perc_test_history = 100*x/n,
            perc_sputum = 100*x1/n,
            perc_xray = 100*x2/n) -> dat_clinic

clinic %>% 
  full_join(dat_clinic) -> dat_clinic
  
ggmap(blt_lines, 
      base_layer = ggplot(dat_clinic)) +
  geom_sf(aes(fill = perc_test_history, lty = "Clinic area"), alpha = 0.5) +
  geom_sf(data = clinics, aes(pch = "TB Clinic")) +
  scale_shape_manual(values = 15) +
  scale_fill_viridis_c(option = "inferno", direction = -1) +
  theme(axis.text = element_blank()) +
  labs(x = "", y = "", 
       fill = "%",
       lty = "", pch = "", 
       subtitle = "% respondents reporting previous test or x-ray by clinic area") 

ggsave(here::here(figdir,"test_history_byclinic.png"), 
       height = 5, width = 6)

# ---------------------------------------------------------------------------- #
# Standardise over age, sex and HIV
# [excluding unknown HIV]

dat$hiv <- dat$hiv_combined
dat$hiv[dat$hiv == "HIV positive ART"] <- "HIV positive"

dat_std <- dat %>% 
  filter(!is.na(agegp) & hiv != "HIV unknown") %>% 
  mutate(agegp2 = cut(s09age, breaks = c(17,30,40,50,max(dat$s09age, na.rm=T))),
         hiv = factor(hiv, levels = c("HIV negative","HIV positive")))

# Calculate standardised rates
dat_std %>% 
  group_by(sex, agegp, hiv) %>% 
  summarise(N = n(),
            last12m = sum(last_test_12m == "Yes"),
            ever = sum(ever_test == "Yes"),
            sputum = sum(s50tb_medtest == 1),
            xray = sum(s53tb_evrxray == 1)) -> total_rates
  # mutate(across(ever:xray, function(x) x/N, .names = "R_{.col}")) 

# By clinic area
dat_std %>% 
  group_by(clinic_id, sex, agegp, hiv) %>% 
  summarise(n = n(),
            n_ever = sum(ever_test == "Yes"),
            n_sputum = sum(s50tb_medtest == 1),
            n_xray = sum(s53tb_evrxray == 1)) %>% 
  ungroup() %>% 
  full_join(total_rates) %>% 
  mutate(across(ever:xray, function(x) x*n/N, .names = "E_{.col}")) %>% 
  group_by(clinic_id) %>% 
  summarise(across(E_ever:E_xray, sum),
            across(n_ever:n_xray, sum)) %>% 
  ungroup() %>% 
  mutate(O_vs_E_ever = n_ever/E_ever,
         O_vs_E_sputum = n_sputum/E_sputum,
         O_vs_E_xray = n_xray/E_xray) -> dat_std_E

# Plot observed over standardised rates
plot_byarea(dat_std_E, clinicarea, varname = "O_vs_E_ever", lab = "O/E", midpoint = 1) +
  labs(title = "Observed versus expected rates of ever testing, by clinic area",
       subtitle = "Standardised by age, sex and HIV")

ggsave(here::here(figdir,"std_test_history_byclinic.png"), 
       height = 5, width = 6)

dat_std_E %>% 
  pivot_longer(O_vs_E_sputum:O_vs_E_xray) %>% 
  mutate(name = factor(name, labels = c("Sputum", "CXR"))) %>% 
  plot_byarea(clinicarea, varname = "value", lab = "O/E", midpoint = 1) +
  facet_wrap(~name) +
  labs(title = "Observed versus expected rates of testing, by type and clinic area",
       subtitle = "Standardised by age, sex and HIV")
ggsave(here::here(figdir,"std_test_history_byclinic_type.png"), 
       height = 5, width = 10)

# By cluster
dat_std %>% 
  group_by(clustid, sex, agegp, hiv) %>% 
  summarise(n = n(),
            n_ever = sum(ever_test == "Yes"),
            n_sputum = sum(s50tb_medtest == 1),
            n_xray = sum(s53tb_evrxray == 1)) %>% 
  ungroup() %>% 
  full_join(total_rates) %>% 
  mutate(across(ever:xray, function(x) x*n/N, .names = "E_{.col}")) %>% 
  group_by(clustid) %>% 
  summarise(across(E_ever:E_xray, sum),
            across(n_ever:n_xray, sum)) %>% 
  ungroup() %>% 
  mutate(O_vs_E_ever = n_ever/E_ever,
         O_vs_E_sputum = n_sputum/E_sputum,
         O_vs_E_xray = n_xray/E_xray) -> dat_std_E

# Plot observed over standardised rates
plot_byarea(dat_std_E, clust, varname = "O_vs_E_ever", lab = "O/E", midpoint = 1) +
  labs(title = "Observed versus expected rates of ever testing, by cluster",
       subtitle = "Standardised by age, sex and HIV")

ggsave(here::here(figdir,"std_test_history_bycluster.png"), 
       height = 5, width = 6)

dat_std_E %>% 
  pivot_longer(O_vs_E_sputum:O_vs_E_xray) %>% 
  mutate(name = factor(name, labels = c("Sputum", "CXR"))) %>% 
  plot_byarea(clust, varname = "value", lab = "O/E", midpoint = 1) +
  facet_wrap(~name) +
  labs(title = "Observed versus expected rates of testing, by type and cluster",
       subtitle = "Standardised by age, sex and HIV")
ggsave(here::here(figdir,"std_test_history_bycluster_type.png"), 
       height = 5, width = 10)


# Tested in last 12m, by cluster
dat_std %>% 
  group_by(clustid, sex, agegp, hiv) %>% 
  summarise(n = n(),
            n_12m = sum(last_test_12m == "Yes")) %>% 
  ungroup() %>% 
  full_join(total_rates) %>% 
  mutate(E_12m = last12m*n/N) %>% 
  group_by(clustid) %>% 
  summarise(E_12m = sum(E_12m),
            n_12m = sum(n_12m)) %>% 
  ungroup() %>% 
  mutate(O_vs_E_12m = n_12m/E_12m) -> dat_std_E

# Plot observed over standardised rates
plot_byarea(dat_std_E, clust, varname = "O_vs_E_12m", lab = "O/E", midpoint = 1) +
  labs(title = "Observed versus expected rates of testing in the last 12m, by cluster",
       subtitle = "Standardised by age, sex and HIV")

ggsave(here::here(figdir,"std_test12m_bycluster.png"), 
       height = 5, width = 6)

# ---------------------------------------------------------------------------- #
# Summarise poverty measures

dat_noHHna <- filter(dat, !is.na(pov_score))

# Plot  scores against each other
dat %>% 
  filter(!is.na(pov_score)) %>% 
  ggplot(aes(as.factor(wealth_step), pov_score)) +
  geom_hline(yintercept = 0, col = "grey", lty = "dashed") +
  geom_boxplot() +
  theme(text = element_text(size = 16)) +
  labs(x = "Self-assessed wealth", y= "PMT poverty score") -> score_by_step

pov_by_clust %>% 
  ggplot(aes(pmt, step)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  theme(text = element_text(size = 16)) +
  labs(y = "Cluster mean self-assessed wealth", x = "Cluster mean PMT score") -> clust_score_by_step

score_by_step + clust_score_by_step + plot_annotation(tag_levels = "A") -> fig3
fig3
ggsave(here::here("figures/manuscript","fig3.png"), 
       height = 5, width = 12, dpi = 350)
ggsave(here::here("figures/manuscript","fig3.pdf"), 
       height = 5, width = 12, dpi = 350)

pmt_overall <- 100*round(Hmisc::binconf(x = sum(dat_noHHna$pov_score > 0), n = nrow(dat_noHHna)),2)
pmt_overall
# PointEst Lower Upper
#        5     4     6

quant1_overall <- 100*round(Hmisc::binconf(x = sum(dat_noHHna$wealth_quant == 1), n = nrow(dat_noHHna)),2)
quant1_overall
# PointEst Lower Upper
#    15     14     17

step1_overall <- 100*round(Hmisc::binconf(x = sum(dat_noHHna$wealth_step == 1), n = nrow(dat_noHHna)),2)
step1_overall
# PointEst Lower Upper
#        7     6     8

# Map scores by cluster

dat %>% 
  filter(!is.na(pov_score)) %>% 
  group_by(clustid) %>% 
  summarise(n = n(),
            tested = sum(ever_test == "Yes"),
            pmt = mean(pov_score),
            step = mean(wealth_step),
            perc_pmt_gt0 = 100*sum(pov_score > 0)/n,
            perc_quant1 = 100*sum(wealth_quant == 1)/n,
            perc_step1 = 100*sum(wealth_step == 1)/n) -> pov_by_clust

pov_by_clust_long <- pov_by_clust %>% 
  pivot_longer(perc_pmt_gt0:perc_step1) %>% 
  mutate(name = factor(name, 
                       levels = c("perc_pmt_gt0", "perc_quant1","perc_step1"),
                       labels = c("PMT score > 0", "PMT score: 1st quantile","Self-assessed wealth: step 1"))) 
pov_by_clust_long %>% 
  ggplot(aes(x = name, y = value)) +
  geom_boxplot() + 
  labs(x = "",y = "Cluster percentage")

ggsave(here::here(figdir,"box_poverty_byclust.png"), 
       height = 5, width = 5)

pov_by_clust_long %>% 
  filter(name != "PMT score: 1st quantile") %>% 
  plot_byarea(areas = clust, varname = "value", lab = "%") +
  facet_wrap(~name) +
  scale_fill_viridis_c(option = "inferno", direction = -1) +
  labs(caption = NULL) +
  theme(text = element_text(size = 16)) -> fig4
fig4
ggsave(here::here("figures/manuscript","fig4.png"), 
       height = 4, width = 8, dpi = 350)
ggsave(here::here("figures/manuscript","fig4.pdf"), 
       height = 4, width = 8, dpi = 350)

#------------------------------------------------------------------------------#
# Spatial autocorrelation in poverty measures

# Specify cluster neighbour structure/weights
nb <- poly2nb(clust, snap = 0.001)

# Cluster 31 has no neighbours - impute as 27 for completeness
nb[[31]] = as.integer(27)

lw <- nb2listw(nb, style="B", zero.policy=TRUE)
M <- nb2mat(nb, style = "B", zero.policy = TRUE)

# Cluster mean PMT score
MC <- moran.mc(pov_by_clust$pmt, lw, nsim=999)
MC
# statistic = 0.13445, observed rank = 559, p-value = 0.06833
par(mfrow = c(1,2))
plot(MC, main="", las=1, xlab = "PMT score - cluster average")

# Cluster mean self-assessed wealth step
MC <- moran.mc(pov_by_clust$step, lw, nsim=999)
MC
# statistic = 0.20652, observed rank = 592, p-value = 0.01333
plot(MC, main="", las=1, xlab = "Self-assessed wealth - cluster average")

#------------------------------------------------------------------------------#
# Testing history by poverty

dat %>% 
  ggplot(aes(pov_score, as.numeric(ever_test == "Yes"))) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  labs(x = "PMT score (logit scale)", y = "Ever tested")

ggplot(dat, aes(x = pov_score, fill = ever_test)) +
  geom_density(position = "stack", col = NA) +
  labs(x = "PMT score (logit scale)", y = "Density", fill = "Ever tested?") +
  guides(fill = "none") -> by_score

ggplot(dat, aes(x = wealth_step, fill = ever_test)) +
  geom_bar(position = "stack") +
  # geom_density() +
  labs(x = "Self-assessed wealth", y = "Count", fill = "Ever tested?") -> by_step

by_score + by_step
ggsave(here::here(figdir, "testhist_by_pov.png"), height = 5, width = 10)

dat %>% 
  filter(!is.na(wealth_step)) %>% 
  group_by(wealth_step) %>% 
  summarise(n = n(),
            x = sum(ever_test == "Yes"),
            p = x/n,
            plo = Hmisc::binconf(x,n,0.1)[2],
            phi = Hmisc::binconf(x,n,0.1)[3]) %>% 
  ggplot(aes(as.factor(wealth_step), p, ymin = plo, ymax = phi)) +
  geom_errorbar(width = 0.3) +
  geom_point() +
  scale_y_continuous(limits = c(0,0.25)) +
  labs(x = "Self-assessed wealth", y = "% ever tested", caption = "with 90% binomial confidence intervals") -> p_by_step

dat %>% 
  filter(!is.na(pov_score)) %>% 
  group_by(wealth_quant) %>% 
  summarise(n = n(),
            x = sum(ever_test == "Yes"),
            p = x/n,
            plo = Hmisc::binconf(x,n,0.1)[2],
            phi = Hmisc::binconf(x,n,0.1)[3]) %>% 
  ggplot(aes(as.factor(wealth_quant), p, ymin = plo, ymax = phi)) +
  geom_errorbar(width = 0.3) +
  geom_point() +
  scale_y_continuous(limits = c(0,0.25)) +
  labs(x = "PMT score quantile", y = "% ever tested", caption = "") -> p_by_quant

p_by_quant + p_by_step
ggsave(here::here(figdir, "testhist_by_pov_cat.png"), height = 5, width = 10)

# By cluster

pov_by_clust %>% 
  ggplot(aes(x = pmt, y = tested/n)) +
 # geom_smooth() +
  geom_point(alpha = 0.5) +
  labs(x = "Cluster mean PMT score", y = "% Ever tested") -> test_pmt_clust
pov_by_clust %>% 
  ggplot(aes(x = step, y = tested/n)) +
 # geom_smooth() +
  geom_point(alpha = 0.5) +
  labs(x = "Cluster mean self-assessed wealth step", y = "") -> test_step_clust

test_pmt_clust + test_step_clust
ggsave(here::here(figdir, "evertest_by_poverty_cluster.png"), height = 5, width = 10)

pov_by_clust %>% 
  ggplot(aes(x = perc_pmt_gt0, y = tested/n)) +
  # geom_smooth() +
  geom_point(alpha = 0.5) +
  labs(x = "Cluster % PMT score > 0", y = "% Ever tested")

pov_by_clust %>% 
  ggplot(aes(x = perc_quant1, y = tested/n)) +
  # geom_smooth() +
  geom_point(alpha = 0.5) +
  labs(x = "Cluster % self-assessed wealth step 1", y = "% Ever tested")

pov_by_clust %>% 
  ggplot(aes(x = perc_step1, y = tested/n)) +
  geom_smooth() +
  geom_point(alpha = 0.5) +
  labs(x = "Cluster mean PMT score", y = "% Ever tested")

################################################################################
################################################################################
