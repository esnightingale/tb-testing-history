plot_bars_byperc <- function(data, Var1, Var2){
  dat %>% 
    mutate(v1 = !!sym(Var1),
           v2 = !!sym(Var2)) %>% 
    group_by(v1,v2) %>% 
    count() %>% 
    group_by(v1) %>% 
    mutate(perc = n*100/sum(n)) %>% 
    ggplot(aes(as.factor(v1), perc, fill = v2)) +
    geom_col() -> p
    return(p)
}
