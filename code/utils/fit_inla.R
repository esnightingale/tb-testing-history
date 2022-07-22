fit_inla <- function(f, lincomb = NULL){
  fit <- INLA::inla(f, 
                    data = dat,
                    family = "binomial",
                    lincomb = lincomb,
                    control.compute = list(waic = TRUE, dic = TRUE, cpo = TRUE),
                    control.predictor = list(compute = TRUE),
                    verbose = FALSE)
  return(fit)
}