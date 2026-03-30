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
#' @param id (Optional) A one-sided formula specifying the clustering variable for
#'   cluster-robust standard errors, e.g. \code{~id}.
#' @param wts (Optional) Weights to be used in the regression. Can be
#'   \code{NULL} (the default), a numeric vector, or the name of a 
#'   column in \code{data} (quoted or unquoted).
#' @param design A \code{survey::svydesign}-object. If a \code{design} is 
#'   provided, \code{cjlm} uses \code{survey::svyglm} as backend using 
#'   the provided design, disregarding other arguments like \code{id},
#'   \code{vcov_type}, and \code{wts}, as all of these are handled
#'   by \code{survey::svyglm} (see \code{?survey::svyglm}).
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
amce <- function(data, formula = NULL, outcome = NULL, attributes = NULL, id = NULL, wts = NULL, design = NULL) {
  
  if (!is.null(formula)) {
    outcome <- deparse(rlang::f_lhs(formula))
    attributes <- labels(terms(formula))
  }
  
  results <- lapply(attributes, function(attr) {
      
      mod <- cjlm(
        data,
        formula = reformulate(attr, response = outcome),
        id = id,
        wts = wts,
        design = design
      )
      
      mod <- mod[mod$term != "(Intercept)", ]
      
      # the first factor level is the baseline & does not appear
      # in the model results; adding baselines back in (we esp.
      # want these to show up in the plots):
      
      all_levels <- levels(data[[attr]])
      
      baseline_row <- data.frame(
        term      = paste0(attr, all_levels[1]),
        estimate  = 0,
        std.error = 0,
        statistic = NA_real_,
        p.value   = NA_real_
      )
      
      # put everything back together
      out <- rbind(baseline_row, mod)
      
      out$attribute <- attr
      out$level <- all_levels
      
      out      
      
    }) |> do.call(rbind, args = _)
  
  results$upper <- results$estimate + results$std.error
  results$lower <- results$estimate - results$std.error
  
  results <- tibble::tibble(results[, c(
    "attribute", "level", "term", "estimate", "std.error", 
    "lower", "upper", "statistic", "p.value"
  )])
  
  class(results) <- c("amce", class(results))
  
  results
  
}

#' Estimate Conditional Average Marginal Component Effects (AMCEs) by Subgroup
#'
#' Computes AMCEs separately for each level of a respondent-level grouping
#' variable.  Conditional AMCEs provide insight into variation in preferences 
#' within groups, but they do not say anything about absolute favorability, and 
#' thus do not provide direct insight about patterns of preferences 
#' between groups (Leeper, Hobolt & Tilley, 2020). For comparing
#' absolute levels of favorability across subgroups, use
#' \code{\link{conditional_marginal_means}} instead.
#' 
#' By default, the function uses \code{stats::lm} to estimate the model, and 
#' \code{sandwich} to adjust standard errors if needed. However, you can also pass
#' a surveydesign (\code{survey::svydesign}), in which case \code{survey::svyglm}
#' will be used.
#'
#' @param data A data frame containing the conjoint data.
#' @param formula A formula of the form \code{outcome ~ attr1 + attr2 + ...}.
#'   If provided, \code{outcome} and \code{attributes} are ignored.
#' @param outcome Character string naming the outcome variable. Ignored if
#'   \code{formula} is provided.
#' @param attributes Character vector of attribute names. Ignored if
#'   \code{formula} is provided.
#' @param id (Optional) A one-sided formula specifying the clustering variable for
#'   cluster-robust standard errors, e.g. \code{~uuid}. If \code{NULL},
#'   standard OLS standard errors are used and a warning is issued.
#' @param group The respondent-level grouping variable (unquoted). AMCEs are
#'   estimated separately for each level of this variable.
#' @param vcov_type The type of heteroskedasticity-consistent covariance
#'   estimator passed to \code{\link[sandwich]{vcovCL}}. Defaults to
#'   \code{"HC1"}.
#' @param wts (Optional) Weights to be used in the regression. Can be
#'   \code{NULL} (the default), a numeric vector, or the name of a 
#'   column in \code{data} (quoted or unquoted).
#' @param design A \code{survey::svydesign}-object. If a \code{design} is 
#'   provided, \code{cjlm} uses \code{survey::svyglm} as backend using 
#'   the provided design, disregarding other arguments like \code{id},
#'   \code{vcov_type}, and \code{wts}, as all of these are handled
#'   by \code{survey::svyglm} (see \code{?survey::svyglm}).
#'
#' @return A data frame of class \code{conditional_amce} with the same columns
#'   as \code{\link{amce}}, plus a column for the grouping variable. The name
#'   of the grouping variable is stored as an attribute on the result and used
#'   by \code{\link{autoplot.conditional_amce}} and
#'   \code{\link{summary.conditional_amce}}.
#'
#' @references Leeper, T. J., Hobolt, S. B., and Tilley, J. (2020). Measuring
#'   Subgroup Preferences in Conjoint Experiments. \emph{Political Analysis},
#'   28(2), 207--221. \doi{10.1017/pan.2019.30}
#'
#' @seealso \code{\link{amce}}, \code{\link{conditional_marginal_means}},
#'   \code{\link{autoplot.conditional_amce}},
#'   \code{\link{summary.conditional_amce}}
#'
#' @examples
#' conditional_amce(
#'   data,
#'   selected ~ group + sex + age,
#'   id    = ~uuid,
#'   group = resp_male
#' )
#'
#' @export
conditional_amce <- function(data, formula = NULL, outcome = NULL, attributes = NULL, id = NULL, group = NULL, wts = NULL, design = NULL) {
  conditional_estimates(
    data, formula, outcome, attributes,
    groupvar   = rlang::as_name(rlang::ensym(group)),
    wts        = wts,
    design     = design,
    .estimator = amce,
    .class     = "conditional_amce"
  )
}

#' @importFrom ggplot2 autoplot
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
    ggplot2::labs(
      x = "AMCE", 
      y = "",
      caption = "*** = p < 0.01, ** = p < 0.05, * = p < 0.1"
    ) +
    ggplot2::facet_wrap(~attribute, ncol = 1, scales = "free_y", space = "free_y")
  
}

#' @importFrom ggplot2 autoplot
#' @export
autoplot.conditional_amce <- function(data, ...) {
  
  group <- attr(data, "group")
  
  data |> 
    ggplot2::ggplot(ggplot2::aes(x = estimate, y = level, color = .data[[group]])) +
    ggplot2::geom_vline(xintercept = 0, lty = "dotted") +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = .4)) +
    ggplot2::geom_linerange(
      ggplot2::aes(xmin = lower, xmax = upper),
      position = ggplot2::position_dodge(width = .4)
    ) +
    ggplot2::labs(x = "AMCE", y = "") +
    ggplot2::facet_wrap(~attribute, ncol = 1, scales = "free_y", space = "free_y")
  
}

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


# The summary for conditional AMCEs is intentionally structured so that
# group comparisons are shown separately, not side-by-side, to reinforce their
# within-group interpretation and to not encourage across-group comparisons.

#' @export
summary.conditional_amce <- function(results, ...) {
  
  group_var <- attr(results, "group")
  groups    <- unique(results[[group_var]])
  
  cat("Conditional Average Marginal Component Effects\n")
  cat(strrep("=", 60), "\n\n")
  
  purrr::walk(groups, function(grp) {
    
    cat(strrep("=", 60), "\n")
    cat(group_var, ":", as.character(grp), "\n")
    cat(strrep("=", 60), "\n\n")
    
    grp_subset <- results[as.character(results[[group_var]]) == as.character(grp), ]
    attrs      <- unique(grp_subset$attribute)
    
    purrr::walk(attrs, function(attr) {
      
      cat("Attribute:", attr, "\n")
      cat("Reference level:", grp_subset$level[as.character(grp_subset$attribute) == attr][1], "\n")
      cat(strrep("-", 60), "\n")
      
      subset <- grp_subset[as.character(grp_subset$attribute) == attr & !is.na(grp_subset$p.value), ]
      
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
    
  })
  
  cat("\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  
}

#' @export
print.amce <- function(x, ...) {
  cat(cli::col_grey("# Average Marginal Component Effects\n\n"))
  NextMethod()
}

#' @export
print.conditional_amce <- function(x, ...) {
  cat(cli::col_grey("# Conditional AMCE\n"))
  cat(cli::col_grey(paste0("# Subgroups by: [", attr(x, "group"), "]\n\n")))
  
  NextMethod()
}
