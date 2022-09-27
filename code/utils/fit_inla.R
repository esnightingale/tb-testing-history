fit_inla <- function(f, data, lincomb = NULL){
  fit <- INLA::inla(f, 
                    data = data,
                    family = "binomial",
                    Ntrials = 1,
                    lincomb = lincomb,
                    control.compute = list(waic = TRUE, dic = TRUE, cpo = TRUE),
                    control.predictor = list(compute = TRUE),
                    verbose = FALSE)
  return(fit)
}