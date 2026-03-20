# Marginal means, conditional marginal means, and autoplot-methods for marginal means.

#' Estimate Marginal Means for a Conjoint Experiment
#' 
#' @param data A data frame containing the conjoint data
#' @param formula A formula of the form `outcome ~ attr1 + attr2 + ...`
#' @param outcome (Alternative to `formula`) Character string naming the outcome variable
#' @param attributes (Alternative to `formula`) Character vector of attribute names
#' @param cluster A one-sided formula specifying the ID/cluster variable, e.g. `~id`
#'
#' @return A data frame of class `marginal_means`
#'
#' @examples
#' marginal_means(data, selected ~ group + sex + age, id = ~id)
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

#' Plot Marginal Means from a Conjoint Experiment
#'
#' Produces a dot-and-whisker plot of marginal means estimates, with one row
#' per attribute level. Error bars represent +/- one standard error. Points
#' are colored by attribute.
#'
#' @param df An object of class \code{marginal_means}, as returned by
#'   \code{\link{marginal_means}}.
#'
#' @return A \code{ggplot2} object.
#'
#' @seealso \code{\link{marginal_means}}
#'
#' @examples
#' mms <- marginal_means(data, selected ~ group + sex + age, cluster = ~uuid)
#' autoplot(mms)
#'
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
