#' Estimate Average Marginal Component Effects (AMCEs) for a Conjoint Experiment
#'
#' Fits a linear probability model and computes AMCEs with
#' cluster-robust standard errors. The reference level for each attribute is
#' included is the first level of each variable. Note that attributes have to
#' be factors.
#'
#' @param data A data frame containing the conjoint data.
#' @param formula A formula of the form \code{outcome ~ attr1 + attr2 + ...}.
#'   If provided, \code{outcome} and \code{attributes} are ignored.
#' @param outcome Character string naming the outcome variable. Ignored if
#'   \code{formula} is provided.
#' @param attributes Character vector of attribute names. Ignored if
#'   \code{formula} is provided.
#' @param id A one-sided formula specifying the clustering variable for
#'   cluster-robust standard errors, e.g. \code{~id}.
#'
#' @return A data frame of class \code{amce}.
#'
#' @seealso \code{\link{autoplot.amce}}, \code{\link{marginal_means}}
#'
#' @examples
#' amce(data, selected ~ group + sex + age, id = ~uuid)
#'
#' # Equivalent using outcome and attributes directly
#' amce(data, outcome = "selected", attributes = c("group", "sex", "age"), id = ~uuid)
#'
#' @export
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

#' Plot Average Marginal Component Effects (AMCEs) from a Conjoint Experiment
#'
#' Produces a dot-and-whisker plot of AMCE estimates, with one row per
#' attribute level. Error bars represent +/- one standard error. Reference
#' levels are plotted at zero with no error bar. Points are colored by
#' attribute.
#'
#' @param df An object of class \code{amce}, as returned by \code{\link{amce}}.
#'
#' @return A \code{ggplot2} object.
#'
#' @seealso \code{\link{amce}}, \code{\link{autoplot.marginal_means}}
#'
#' @examples
#' amces <- amce(data, selected ~ group + sex + age, id = ~uuid)
#' autoplot(amces)
#'
#' @export
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

