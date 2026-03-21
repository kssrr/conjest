# Marginal means, conditional marginal means, and autoplot-methods for marginal means.

#' Estimate Marginal Means for a Conjoint Experiment
#' 
#' @param data A data frame containing the conjoint data
#' @param formula A formula of the form `outcome ~ attr1 + attr2 + ...`
#' @param outcome (Alternative to `formula`) Character string naming the outcome variable
#' @param attributes (Alternative to `formula`) Character vector of attribute names
#' @param id A one-sided formula specifying the ID/cluster variable, e.g. `~id`
#' @param vcov_type Type of variance-covariance estimation when clustering (HC0-HC3). Default is "HC1".
#'
#' @return A data frame of class `marginal_means`
#'
#' @examples
#' marginal_means(data, selected ~ group + sex + age, id = ~id)
#' @export
marginal_means <- function(data, formula = NULL, outcome = NULL, attributes = NULL, id = NULL, vcov_type = "HC1") {
  
  if (!is.null(formula)) {
    outcome    <- deparse(rlang::f_lhs(formula))
    attributes <- labels(terms(formula))
  }
  
  validate_inputs(data, attributes, id)
  
  # fit models:
  
  mods <- attributes |> lapply(function(x) {
    lm(reformulate(x, response = outcome, intercept = FALSE), data = data)
  })
  
  # cluster SEs & tidy up results:
  
  results <- 
    purrr::map2(mods, attributes, function(model, attr) {
      
      if (!is.null(id)) {
        
        processed <- lmtest::coeftest(
          model,
          vcov. = sandwich::vcovCL(
            model,
            cluster = id,
            type = vcov_type
          )
        )
        
      } else {
        
        processed <- lmtest::coeftest(model)
        
      }
      
      processed |> 
        broom::tidy() |> 
        dplyr::bind_cols(attribute = attr, level = model$xlevels[[attr]])
      
    }) |>
    dplyr::bind_rows() |> 
    dplyr::mutate(lower = estimate - std.error, upper = estimate + std.error) |> 
    dplyr::select(attribute, level, term, estimate, std.error, lower, upper)
  
  class(results) <- c("marginal_means", class(results))
  
  results
  
}

#' Plot Marginal Means from a Conjoint Experiment
#'
#' Produces a dot-and-whisker plot of marginal means estimates, with one row
#' per attribute level. Error bars represent the 95% CI. Points
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

#' @export
print.marginal_means <- function(x, ...) {
  cat(cli::col_grey("# Marginal Means\n\n"))
  NextMethod()
}