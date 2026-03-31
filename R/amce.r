#' Estimate Average Marginal Component Effects (AMCEs) for a Conjoint Experiment
#'
#' Computes average marginal component effects (AMCEs) for each level of each
#' attribute in a conjoint experiment. The AMCE represents the causal effect of
#' a given attribute level on the outcome, typically the probability of a
#' profile being chosen, relative to a baseline level, averaging over the
#' joint distribution of all other attributes. AMCEs have a clear causal
#' interpretation under the randomisation of the conjoint design, as introduced
#' by Hainmueller, Hopkins, and Yamamoto (2014). The baseline level for each
#' attribute is included in the output with an estimate of zero. Note that
#' AMCEs are defined relative to the chosen reference category, and are thus 
#' sensitive to the choice of reference category.
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
#' @param wts (Optional) Weights to be used in the regression, as a one-sided
#'   formula, e.g. \code{~weights}
#' @param design A \code{survey::svydesign}-object. If a \code{design} is 
#'   provided, \code{id} and \code{wts} are ignored, and adjustments are made
#'   based on the provided design instead.
#'
#' @return A data frame of class \code{amce}.
#'
#' @references
#'   Hainmueller, J., Hopkins, D. J., and Yamamoto, T. (2014). Causal Inference
#'   in Conjoint Analysis: Understanding Multidimensional Choices via Stated
#'   Preference Experiments. \emph{Political Analysis}, 22(1), 1--30.
#'   \doi{10.1093/pan/mpt024}
#' 
#' @seealso \code{\link{conditional_amce}}, \code{\link{marginal_means}}
#'
#' @examples
#' library(conjest)
#' data("immigration")
#' 
#' immigration |> amce(ChosenImmigrant ~ Education + Gender, id = ~CaseID)
#'
#' @export
amce <- function(data, formula = NULL, outcome = NULL, attributes = NULL, id = NULL, wts = NULL, design = NULL) {
  
  if (!is.null(formula)) {
    outcome <- deparse(rlang::f_lhs(formula))
    attributes <- labels(terms(formula))
  }

  assert_fct(data, attributes)
  
  results <- lapply(attributes, function(attr) {
      
      mod <- cjlm(
        data,
        formula = reformulate(attr, response = outcome),
        id      = id,
        wts     = wts,
        design  = design
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
      out$level     <- all_levels
      
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
#' within groups, but they do not say anything about absolute favorability (as
#' AMCEs are sensitive to the choice of the reference category), and 
#' thus do not provide direct insight about patterns of preferences 
#' between groups (Leeper, Hobolt & Tilley, 2020). For comparing
#' absolute levels of favorability across subgroups, use
#' \code{\link{conditional_marginal_means}} instead.
#' 
#' @param data A data frame containing the conjoint data.
#' @param formula A formula of the form \code{outcome ~ attr1 + attr2 + ...}.
#'   If provided, \code{outcome} and \code{attributes} are ignored.
#' @param outcome Character string naming the outcome variable. Ignored if
#'   \code{formula} is provided.
#' @param attributes Character vector of attribute names. Ignored if
#'   \code{formula} is provided.
#' @param id (Optional) A one-sided formula specifying the clustering variable for
#'   cluster-robust standard errors, e.g. \code{~uuid}.
#' @param group The respondent-level grouping variable (unquoted). AMCEs are
#'   estimated separately for each level of this variable.
#' @param wts (Optional) Weights to be used in the regression, as a one-sided
#'   formula, e.g. \code{~weights}.
#' @param design A \code{survey::svydesign}-object. If a \code{design} is 
#'   provided, \code{id} and \code{wts} are ignored, and adjustments are made 
#'   based on the provided design instead.
#'
#' @return A data frame of class \code{conditional_amce} with the same columns
#'   as \code{\link{amce}}, plus a column for the grouping variable.
#'
#' @references Leeper, T. J., Hobolt, S. B., and Tilley, J. (2020). Measuring
#'   Subgroup Preferences in Conjoint Experiments. \emph{Political Analysis},
#'   28(2), 207--221. \doi{10.1017/pan.2019.30}
#'
#' @seealso \code{\link{amce}}, \code{\link{conditional_marginal_means}}
#'
#' @examples
#' \dontrun{
#'   conditional_amce(
#'     data,
#'     selected ~ group + sex + age,
#'     id    = ~uuid,
#'     group = resp_sex
#'   )
#' }
#'
#' @export
conditional_amce <- function(data, formula = NULL, outcome = NULL, attributes = NULL, id = NULL, group = NULL, wts = NULL, design = NULL) {
  conditional_estimates(
    data, formula, outcome, attributes,
    groupvar   = rlang::as_name(rlang::ensym(group)),
    id         = id,
    wts        = wts,
    design     = design,
    .estimator = amce,
    .class     = "conditional_amce"
  )
}

#' @export
autoplot.amce <- function(object, ...) {
  
  object$sig <- make_stars(
    object$p.value, 
    thresholds = c(      .01,     .05,    .1   ), 
    labels     = c("***",    "**",    "*",   "")
  )
  
  object |> 
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

#' @export
autoplot.conditional_amce <- function(object, ...) {
  
  group <- attr(object, "group")
  
  object |> 
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
summary.amce <- function(object, ...) {
  
  attrs <- unique(object$attribute)
  
  cat("Average Marginal Component Effects\n")
  cat(strrep("=", 60), "\n\n")
  
  for (attr in attrs) {
    
    cat("Attribute:", attr, "\n")
    cat("Reference level:", object$level[as.character(object$attribute) == attr][1], "\n")
    cat(strrep("-", 60), "\n")
    
    subset <- object[as.character(object$attribute) == attr & !is.na(object$p.value), ]
    subset$stars <- make_stars(subset$p.value)
    
    numeric_cols     <- c("estimate", "std.error", "statistic", "p.value")
    subset[numeric_cols] <- lapply(subset[numeric_cols], format_number)
    
    out <- data.frame(
      ` `          = subset$level,
      `Estimate`   = subset$estimate,
      `Std. Error` = subset$std.error,
      `t value`    = subset$statistic,
      `Pr(>|t|)`   = subset$p.value,
      ` `          = subset$stars,
      check.names  = FALSE
    )
    
    print(out, row.names = FALSE)
    cat("\n")
    
  }
  
  cat("\nSignif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  
}


# The summary for conditional AMCEs is intentionally structured so that
# group comparisons are shown separately, not side-by-side, to reinforce their
# within-group interpretation and to not encourage across-group comparisons.

#' @export
summary.conditional_amce <- function(object, ...) {
  
  group_var <- attr(object, "group")
  groups    <- unique(object[[group_var]])
  
  cat("Conditional Average Marginal Component Effects\n")
  cat(strrep("=", 60), "\n\n")
  
  for (grp in groups) {
    
    cat(strrep("=", 60), "\n")
    cat(group_var, ":", as.character(grp), "\n")
    cat(strrep("=", 60), "\n\n")
    
    grp_subset <- object[as.character(object[[group_var]]) == as.character(grp), ]
    attrs      <- unique(grp_subset$attribute)
    
    for (attr in attrs) {
      
      cat("Attribute:", attr, "\n")
      cat("Reference level:", grp_subset$level[as.character(grp_subset$attribute) == attr][1], "\n")
      cat(strrep("-", 60), "\n")
      
      subset <- grp_subset[as.character(grp_subset$attribute) == attr & !is.na(grp_subset$p.value), ]
      
      out <- data.frame(
        ` `          = subset$level,
        `Estimate`   = formatC(subset$estimate,  format = "f", digits = 4),
        `Std. Error` = formatC(subset$std.error, format = "f", digits = 4),
        `t value`    = formatC(subset$statistic, format = "f", digits = 3),
        `Pr(>|t|)`   = formatC(subset$p.value,   format = "e", digits = 2),
        ` `          = make_stars(subset$p.value),
        check.names  = FALSE
      )
      
      print(out, row.names = FALSE)
      cat("\n")
      
    }
    
  }
  
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
