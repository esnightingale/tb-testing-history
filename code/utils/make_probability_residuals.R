library(rlang)
make_probability_residuals = function(data, prediction, y, y_upper = NA, n = 1) {
  .prediction = enquo(prediction)
  .y = enquo(y)
  .y_upper = enquo(y_upper)
  
  if (eval_tidy(expr(is.factor(!!.prediction) && !is.ordered(!!.prediction)), data)) {
    data = mutate(data, !!.prediction := ordered(!!.prediction, levels = levels(!!.prediction)))
  }
  
  if (is.na(enquo(y_upper)[[2]])) {
    #no y_upper provided, use y as y_upper
    data = summarise(data,
                     .p_lower = mean(!!.prediction < !!.y),
                     .p_upper = mean(!!.prediction <= !!.y),
                     .groups = "drop_last"
    )
  } else {
    #y_upper should be a vector, and if an entry in it is NA, use the entry from y
    data = summarise(data,
                     .p_lower = mean(!!.prediction < !!.y),
                     .p_upper = mean(!!.prediction <= ifelse(is.na(!!.y_upper), !!.y, !!.y_upper)),
                     .groups = "drop_last"
    )
  }
  
  data %>%
    mutate(
      .p_residual = map2(.p_lower, .p_upper, runif, n = !!n),
      .residual_draw = map(.p_residual, seq_along)
    ) %>%
    unnest(c(.p_residual, .residual_draw)) %>%
    mutate(.z_residual = qnorm(.p_residual))
}