# Marginal means, conditional marginal means, and autoplot-methods for marginal means.

#' @export
marginal_means <- function(data, formula = NULL, outcome = NULL, attributes = NULL, id) {
  
  if (!is.null(formula)) {
    outcome    <- deparse(rlang::f_lhs(formula))
    attributes <- labels(terms(formula))
  }
  
  mods <- attributes |> lapply(function(x) {
    lm(reformulate(x, response = outcome, intercept = FALSE), data = data)
  })
  
  results <- 
    Map(function(model, attr) {
      
      tidy_mod <- model |>
        lmtest::coeftest(vcov. = sandwich::vcovCL(model, cluster = id)) |>
        broom::tidy()
      
      tidy_mod$attribute <- attr
      tidy_mod$level     <- model$xlevels[[attr]]
      tidy_mod
      
    }, mods, attributes) |>
    do.call(rbind, args = _) |>
    _[, c("attribute", "level", "term", "estimate", "std.error")]
  
  class(results) <- c("marginal_means", class(results))
  
  results
  
}

#' @export
autoplot.marginal_means <- function(df) {
  
  df |> 
    dplyr::mutate(
      lower = estimate - std.error,
      upper = estimate + std.error,
      label = paste0(stringr::str_to_title(attribute), ": ", level)
    ) |> 
    ggplot2::ggplot(ggplot2::aes(x = estimate, y = label, color = attribute)) +
    ggplot2::geom_vline(xintercept = .5, lty = "dotted") +
    ggplot2::geom_point() +
    ggplot2::geom_linerange(ggplot2::aes(xmin = lower, xmax = upper)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Marginal Mean", y = "")
  
}

