std_by_hiv <- function(dat_std, lab = NULL, save = TRUE){
  
  dat_std %>% 
    group_by(sex, agegp, hiv) %>% 
    summarise(N = n(),
              last12m = sum(last_test_12m == "Yes"),
              ever = sum(ever_test == "Yes"),
              sputum = sum(s50tb_medtest == 1),
              xray = sum(s53tb_evrxray == 1)) -> total_rates
  
  # print(
  #   summary(total_rates)
  # )
  
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
           O_vs_E_xray = n_xray/E_xray) -> dat_std_E_clinic
  
  # print(
  #   dat_std_E_clinic %>% 
  #   dplyr::select(starts_with("O_vs_E")) %>% 
  #   summary()
  # )
  
  # Plot observed over standardised rates
  plot_byarea(dat_std_E_clinic, clinicarea, varname = "O_vs_E_ever", lab = "O/E", midpoint = 1) +
    labs(title = "Observed versus expected rates of ever testing, by clinic area",
         subtitle = "Standardised by age, sex and HIV")
  
  if(save == T){
    ggsave(here::here(figdir,paste0("stdR_byclinic",lab,".png")), 
           height = 5, width = 6)
  }
  
  dat_std_E_clinic %>% 
    pivot_longer(O_vs_E_sputum:O_vs_E_xray) %>% 
    mutate(name = factor(name, labels = c("Sputum", "CXR"))) %>% 
    plot_byarea(clinicarea, varname = "value", lab = "O/E", midpoint = 1) +
    facet_wrap(~name) +
    labs(title = "Observed versus expected rates of testing, by type and clinic area",
         subtitle = "Standardised by age, sex and HIV")
  
  if(save == T){
    ggsave(here::here(figdir,paste0("stdR_byclinic_type",lab,".png")), 
           height = 5, width = 10)
  }
  
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
           O_vs_E_xray = n_xray/E_xray) -> dat_std_E_cluster
  
  print(
    dat_std_E_cluster %>% 
      dplyr::select(starts_with("O_vs_E")) %>% 
      summary()
  )

  
  # Plot observed over standardised rates
  plot_byarea(dat_std_E_cluster, clust, varname = "O_vs_E_ever", lab = "O/E", midpoint = 1) +
    labs(title = "Observed versus expected rates of ever testing, by cluster",
         subtitle = "Standardised by age, sex and HIV")
  
  if(save == T){
    ggsave(here::here(figdir,paste0("stdR_bycluster",lab,".png")), 
           height = 5, width = 6)
  }
  
  dat_std_E_cluster %>% 
    pivot_longer(O_vs_E_sputum:O_vs_E_xray) %>% 
    mutate(name = factor(name, labels = c("Sputum", "CXR"))) %>% 
    plot_byarea(clust, varname = "value", lab = "O/E", midpoint = 1) +
    facet_wrap(~name) +
    labs(title = "Observed versus expected rates of testing, by type and cluster",
         subtitle = "Standardised by age, sex and HIV")
  
  if(save == T){
    ggsave(here::here(figdir,paste0("stdR_bycluster_type",lab,".png")), 
         height = 5, width = 10)
  }
  
  
}