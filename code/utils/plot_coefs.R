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