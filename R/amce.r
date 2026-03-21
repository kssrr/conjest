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
#' @param vcov_type Type of variance-covariance estimation when clustering (HC0-HC3). Default is "HC1".
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
amce <- function(data, formula = NULL, outcome = NULL, attributes = NULL, id = NULL, vcov_type = "HC1") {
  
  if (!is.null(formula)) {
    outcome    <- deparse(rlang::f_lhs(formula))
    attributes <- labels(terms(formula))
  }
  
  validate_inputs(data, attributes, id)
  
  # fit models separately (could be one model but this
  # makes processing a bit more streamlined):
  
  mods <- attributes |> lapply(function(x) {
    lm(reformulate(x, response = outcome), data = data)
  })
  
  results <- 
    purrr::map2(mods, attributes, function(model, attr) {
    
      # cluster SEs if `id` was provided:
      
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
      
      # the first factor level is the baseline & does not appear
      # in the model results; adding baselines back in (we esp.
      # want these to show up in the plots):
      
      all_levels     <- model$xlevels[[attr]]
      baseline_level <- all_levels[1]
      
      baseline_row <- data.frame(
        term      = paste0(attr, baseline_level),
        estimate  = 0,
        std.error = 0,
        statistic = 0,
        p.value = NA_real_
      )
      
      # put everything together & tidy up:
      
      processed |> 
        broom::tidy() |> 
        dplyr::filter(term != "(Intercept)") |> 
        dplyr::add_row(baseline_row, .before = 1) |> 
        #^ have to *pre*pend this one for row order to match:
        dplyr::mutate(attribute = attr, level = all_levels)
    
    }) |> 
    dplyr::bind_rows() |> 
    dplyr::mutate(lower = estimate - std.error, upper = estimate + std.error) |> 
    dplyr::select(
      term, estimate, std.error, lower, upper,
      statistic, p.value, attribute, level
    )
  
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
      sig = dplyr::case_when(
        p.value < .01 ~ "***",
        p.value < .05 ~ "**",
        p.value < .1 ~ "*",
        TRUE ~ ""
      )
    ) |> 
    ggplot2::ggplot(ggplot2::aes(x = estimate, y = level, color = attribute)) +
    ggplot2::geom_vline(xintercept = 0, lty = "dotted") +
    ggplot2::geom_point() +
    ggplot2::geom_text(
      ggplot2::aes(label = sig),
      position = ggplot2::position_nudge(y = .15)
    ) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = lower, xmax = upper)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "AMCE", y = "") +
    ggplot2::facet_wrap(~attribute, ncol = 1, scales = "free_y", space = "free_y")
  
}

#' Summarize Average Marginal Component Effects
#'
#' Prints a formatted summary of an \code{amce} object, grouped by attribute,
#' with coefficient estimates, standard errors, t-statistics, p-values, and
#' significance stars. The reference level for each attribute is printed as a
#' header and omitted from the coefficient table.
#'
#' @param x An object of class \code{amce}, as returned by \code{\link{amce}}.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns \code{x}. Called for its side effect of printing
#'   a formatted summary to the console.
#'
#' @seealso \code{\link{amce}}, \code{\link{autoplot.amce}}
#'
#' @examples
#' amces <- amce(data, selected ~ group + sex + age, id = ~uuid)
#' summary(amces)
#'
#' @export
summary.amce <- function(results, ...) {
  
  attrs <- unique(results$attribute)
  
  cat("Average Marginal Component Effects\n")
  cat(strrep("=", 60), "\n\n")
  
  purrr::walk(attrs, function(attr) {
    
    cat("Attribute:", attr, "\n")
    cat("Reference level:", results$level[as.character(results$attribute) == attr][1], "\n")
    cat(strrep("-", 60), "\n")
    
    subset <- results[as.character(results$attribute) == attr & !is.na(results$p.value), ]
    
    stars <- dplyr::case_when(
      subset$p.value < 0.001 ~ " ***",
      subset$p.value < 0.01  ~ " ** ",
      subset$p.value < 0.05  ~ " *  ",
      subset$p.value < 0.1   ~ " .  ",
      TRUE                   ~ "    "
    )
    
    out <- data.frame(
      ` `          = subset$level,
      `Estimate`   = formatC(subset$estimate,  format = "f", digits = 4),
      `Std. Error` = formatC(subset$std.error, format = "f", digits = 4),
      `t value`    = formatC(subset$statistic, format = "f", digits = 3),
      `Pr(>|t|)`   = formatC(subset$p.value,   format = "e", digits = 2),
      ` `          = stars,
      check.names  = FALSE
    )
    
    print(out, row.names = FALSE)
    cat("\n")
    
  })
  
  cat("\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  
}