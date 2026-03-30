# Marginal means, conditional marginal means, and autoplot-methods for marginal means.

#' Estimate Marginal Means for a Conjoint Experiment
#'
#' Computes marginal means (MMs) for each level of each attribute in a conjoint
#' experiment. A marginal mean represents the average outcome, typically the
#' probability of a profile being chosen, when a given attribute level is
#' present, averaging over all other attributes and their levels. Unlike AMCEs,
#' marginal means are not defined relative to a reference category, making them
#' better suited for describing absolute levels of support and comparing
#' preferences across subgroups. Values above 0.5 indicate that a level
#' increases the probability of selection relative to the average profile, and
#' values below 0.5 indicate the opposite.
#' 
#' By default, the function uses \code{stats::lm} to estimate the model, and 
#' \code{sandwich} to adjust standard errors if needed. However, you can also pass
#' a surveydesign (\code{survey::svydesign}), in which case \code{survey::svyglm}
#' will be used.
#' 
#' @param data A data frame containing the conjoint data
#' @param formula A formula of the form `outcome ~ attr1 + attr2 + ...`
#' @param outcome (Alternative to `formula`) Character string naming the outcome variable
#' @param attributes (Alternative to `formula`) Character vector of attribute names
#' @param id A one-sided formula specifying the ID/cluster variable, e.g. `~id`
#' @param vcov_type Type of variance-covariance estimation when clustering (HC0-HC3). Default is "HC1".
#' @param wts (Optional) Weights to be used in the regression. Can be
#'   \code{NULL} (the default), a numeric vector, or the name of a 
#'   column in \code{data} (quoted or unquoted).
#' @param design A \code{survey::svydesign}-object. If a \code{design} is 
#'   provided, \code{cjlm} uses \code{survey::svyglm} as backend using 
#'   the provided design, disregarding other arguments like \code{id},
#'   \code{vcov_type}, and \code{wts}, as all of these are handled
#'   by \code{survey::svyglm} (see \code{?survey::svyglm}).
#'
#' @return A data frame of class `marginal_means`
#'
#' @examples
#' marginal_means(data, selected ~ group + sex + age, id = ~id)
#' @export
marginal_means <- function(data, formula = NULL, outcome = NULL, attributes = NULL, id = NULL, wts = NULL, design = NULL) {
  
  if (!is.null(formula)) {
    outcome    <- deparse(rlang::f_lhs(formula))
    attributes <- labels(terms(formula))
  }
  
  assert_fct(data, attributes)
  
  results <- lapply(attributes, function(attr) {
    
    mod <- cjlm(
      data,
      formula   = reformulate(attr, response = outcome, intercept = FALSE),
      id        = id,
      wts       = wts,
      design    = design
    )
    
    mod$attribute <- attr
    mod$level <- levels(data[[attr]])
    
    mod
    
  }) |> do.call(rbind, args = _)
    
  results$lower <- results$estimate - results$std.error
  results$upper <- results$estimate + results$std.error
  
  results <- tibble::tibble(results[, c(
    "attribute", "level", "term", "estimate", 
    "std.error", "lower", "upper"
  )])
  
  class(results) <- c("marginal_means", class(results))
  
  results
  
}

#' Estimate Conditional Marginal Means for a Conjoint Experiment
#' 
#' Conditional version of \code{\link{marginal_means}}. Computes marginal means separately for each 
#' level of a respondent-level grouping variable, allowing comparisons of conjoint preferences across
#' subgroups. This approach is preferred over conditional AMCEs for subgroup analysis, as conditional
#' marginal means do not require a baseline, and are thus not sensitive to the choice of a baseline
#' category. This was recommended by Leeper, Hobolt & Tilley (2020).
#' 
#' By default, the function uses \code{stats::lm} to estimate the model, and 
#' \code{sandwich} to adjust standard errors if needed. However, you can also pass
#' a surveydesign (\code{survey::svydesign}), in which case \code{survey::svyglm}
#' will be used.
#' 
#' @param data A data frame containing the conjoint data
#' @param formula A formula of the form `outcome ~ attr1 + attr2 + ...`
#' @param outcome (Alternative to `formula`) Character string naming the outcome variable
#' @param attributes (Alternative to `formula`) Character vector of attribute names
#' @param id (Optional) A one-sided formula specifying the ID/cluster variable, e.g. `~id`
#' @param group The respondent-level grouping variable (unquoted). Marginal
#'   means are estimated separately for each level of this variable.
#' @param vcov_type Type of variance-covariance estimation when clustering (HC0-HC3). Default is "HC1".
#' @param wts (Optional) Weights to be used in the regression. Can be
#'   \code{NULL} (the default), a numeric vector, or the name of a 
#'   column in \code{data} (quoted or unquoted).
#' @param design A \code{survey::svydesign}-object. If a \code{design} is 
#'   provided, \code{cjlm} uses \code{survey::svyglm} as backend using 
#'   the provided design, disregarding other arguments like \code{id},
#'   \code{vcov_type}, and \code{wts}, as all of these are handled
#'   by \code{survey::svyglm} (see \code{?survey::svyglm}).
#'
#' @return A data frame of class `marginal_means`
#' 
#' @references Leeper, T. J., Hobolt, S. B., and Tilley, J. (2020). Measuring
#'   Subgroup Preferences in Conjoint Experiments. \emph{Political Analysis},
#'   28(2), 207--221. \doi{10.1017/pan.2019.30}
#'
#' @examples
#' marginal_means(data, selected ~ group + sex + age, id = ~id)
#' @export
conditional_marginal_means <- function(data, formula = NULL, outcome = NULL, attributes = NULL, id = NULL, group = NULL, vcov_type = "HC1", wts = NULL, design = NULL) {
  
  # build the design here bc we need to subset it before
  # passing to `amce` which passes it to `cjlm`
  
  full_design <- validate_design(data, design, id, wts)
  
  groupvar <- deparse(substitute(group))
  groups <- unique(data[[groupvar]])
  
  result <- lapply(groups, function(g) {
    
    sub_design <- full_design[full_design$variables[[groupvar]] == g, ]
    
    sub_res <- marginal_means(
      data,
      formula = formula,
      outcome = outcome,
      attributes = attributes,
      design = sub_design
    )
    
    sub_res[[groupvar]] <- g
    
    sub_res
        
  }) |> do.call(rbind, args = _)
   
  class(result) <- c("conditional_marginal_means", class(result))
  attr(result, "group") <- rlang::as_name(rlang::ensym(group))
  
  result
  
}

#' @importFrom ggplot2 autoplot
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

#' @importFrom ggplot2 autoplot
#' @export
autoplot.conditional_marginal_means <- function(data, ...) {
  
  group <- attr(data, "group")
  
  data |> 
    ggplot2::ggplot(ggplot2::aes(x = estimate, y = level, color = .data[[group]])) +
    ggplot2::geom_vline(xintercept = .5, lty = "dotted") +
    ggplot2::geom_point(position = ggplot2::position_dodge(width = .4)) +
    ggplot2::geom_linerange(
      ggplot2::aes(xmin = lower, xmax = upper),
      position = ggplot2::position_dodge(width = .4)
    ) +
    ggplot2::labs(x = "Marginal Mean", y = "") +
    ggplot2::facet_wrap(~attribute, ncol = 1, scales = "free_y", space = "free_y")
  
}

#' @export
summary.marginal_means <- function(data, ...) {
  
  attrs <- unique(data$attribute)
  
  cat("Marginal Means\n")
  cat(strrep("=", 60), "\n\n")
  
  purrr::walk(attrs, function(attr) {
    
    cat("Attribute:", attr, "\n")
    cat(strrep("-", 60), "\n")
    
    subset <- data[data$attribute == attr, ]
    
    out <- data.frame(
      ` `          = subset$level,
      `Estimate`   = formatC(subset$estimate,  format = "f", digits = 4),
      `Std. Error` = formatC(subset$std.error, format = "f", digits = 4),
      ` `          = ifelse(subset$estimate < .5, "-", "+"),
      check.names  = FALSE
    )
    
    print(out, row.names = FALSE)
    cat("\n")
    
  })
}

#' @export
summary.conditional_marginal_means <- function(data, ...) {
  
  group_var <- attr(data, "group")
  attrs     <- unique(data$attribute)
  
  cat("Conditional Marginal Means\n")
  cat(strrep("=", 60), "\n\n")
  
  purrr::walk(attrs, function(attr) {
    
    cat("Attribute:", attr, "\n")
    cat(strrep("-", 60), "\n")
    
    attr_data <- data[as.character(data$attribute) == attr, ]
    levels    <- unique(attr_data$level)
    
    purrr::walk(levels, function(lvl) {
      
      cat("Level:", lvl, "\n\n")
      
      subset <- attr_data[attr_data$level == lvl, ]
      
      out <- data.frame(
        ` `            = as.character(subset[[group_var]]),
        `Estimate`     = formatC(subset$estimate,  format = "f", digits = 4),
        `Std. Error`   = formatC(subset$std.error, format = "f", digits = 4),
        ` `            = ifelse(subset$estimate >= 0.5, "+", "-"),
        check.names    = FALSE
      )
      
      print(out, row.names = FALSE)
      cat("\n")
      
    })
    
    cat(strrep("-", 60), "\n\n")
    
  })
  
}

#' @export
print.marginal_means <- function(x, ...) {
  cat(cli::col_grey("# Marginal Means\n\n"))
  NextMethod()
}

#' @export
print.conditional_marginal_means <- function(x, ...) {
  
  group <- attr(x, "group")
  
  cat(cli::col_grey("# Conditional Marginal Means\n"))
  cat(cli::col_grey(paste0("# Subgroups by: [", group, "]\n\n")))
  NextMethod()
}