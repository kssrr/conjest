amce <- function(data, formula = NULL, outcome = NULL, attributes = NULL, id) {
  
  if (!is.null(formula)) {
    outcome    <- deparse(rlang::f_lhs(formula))
    attributes <- labels(terms(formula))
  }
  
  mods <- attributes |> lapply(function(x) {
    lm(reformulate(x, response = outcome), data = data)
  })
  
  results <- 
    Map(function(model, attr) {
    
    tidy_mod <- model |>
      lmtest::coeftest(vcov. = sandwich::vcovCL(model, cluster = id)) |>
      broom::tidy()
    
    all_levels     <- model$xlevels[[attr]]
    baseline_level <- all_levels[1]
    
    baseline_row <- data.frame(
      term      = paste0(attr, baseline_level),
      estimate  = 0,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_
    )
    
    tidy_mod <- tidy_mod[tidy_mod$term != "(Intercept)", ]
    
    result           <- rbind(baseline_row, tidy_mod)
    result$attribute <- attr
    result$level     <- all_levels
    
    result
    
  }, mods, attributes) |> do.call(rbind, args = _)
  
  class(results) <- c("amce", class(results))
  
  results
  
}

autoplot.amce <- function(df) {
  
  df |> 
    dplyr::mutate(
      lower = estimate - std.error,
      upper = estimate + std.error,
      label = paste0(attribute, ": ", level)
    ) |> 
    ggplot2::ggplot(ggplot2::aes(x = estimate, y = label, color = attribute)) +
    ggplot2::geom_vline(xintercept = 0, lty = "dotted") +
    ggplot2::geom_point() +
    ggplot2::geom_linerange(ggplot2::aes(xmin = lower, xmax = upper)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "AMCE", y = "")
  
}

