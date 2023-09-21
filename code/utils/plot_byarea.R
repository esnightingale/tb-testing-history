plot_byarea <- function(dat, areas, varname, 
                        lab = NULL, midpoint = NULL,
                        low = "red", high = "blue"){
  
  dat <- areas %>% 
    full_join(dat) %>% 
    mutate(v = !!sym(varname))
  
  if (is.null(lab)){lab = varname}
  
  if (is.null(midpoint)){
    midpoint = mean(dat$v, na.rm = T)
    caption = "Scale centred on mean value across all areas."
  }else{
   caption = paste0("Scale centred on ",midpoint)
   }
  
  ggmap(blt_lines, 
        base_layer = ggplot(dat)) +
    geom_sf(aes(fill = v), alpha = 1) +
    scale_fill_gradient2(midpoint = midpoint, low = low, high = high) +
    labs(fill = lab,
         caption = caption) +
    theme(axis.text = element_blank()) -> p
  
  return(p)
  
}
