tab_fac <- function(Var1, nm, Var2){
  
  tab <- table(Var1, Var2)
  # print(round(prop.table(tab)*100, 2))
  test <- chisq.test(tab, simulate.p.value = T)
  
  # Add NA factor level for output
  Var1 <- addNA(Var1)
  tab <- table(Var1, Var2)
  tab %>% 
    as.data.frame() %>% 
    mutate(Var = nm,
           p = round(test$p.value,4)) %>% 
    dplyr::select(Var, everything()) %>% 
    return()
}

make_tab <- function(fact_vars, Var2){

tabs1 <- purrr::map2(fact_vars, names(fact_vars), tab_fac, Var2 = Var2)

tab <- bind_rows(tabs1) %>% 
  pivot_wider(names_from = "Var2", values_from = "Freq") %>% 
  mutate(N = No + Yes,
         summ_no = paste0(No, " (",round(No*100/(No + Yes),1), ")"),
         summ_yes = paste0(Yes, " (",round(Yes*100/(No + Yes),1), ")")) %>% 
  dplyr::select(Var, Var1, N, summ_yes, p)

}
