plot_ci <- function(dat, varname, valname, excl_na = TRUE){

dat %>% 
    mutate(var = as.factor(!!sym(varname)),
           val = !!sym(valname)) %>%
  group_by(var) %>% 
  summarise(n = n(),
            p = 100*sum(val=="Yes")/n,
            low = 100*Hmisc::binconf(sum(val=="Yes"), n)[2],
            high = 100*Hmisc::binconf(sum(val=="Yes"), n)[3]) -> byvar
  
  if (excl_na == TRUE){
    byvar <- filter(byvar, !is.na(var))
  }
  
ggplot(byvar, aes(x = var, y = p, ymin = low, ymax = high)) +
  geom_hline(yintercept = 100*mean(pull(dat, !!sym(valname)) == "Yes"), col = "red", lty = "dashed") +
  geom_point() +
  geom_errorbar(width = 0.2) + 
  labs(x = varname,
       y = "%") -> p

#caption = "Overall percentage shown in red."

return(p)

}
  

