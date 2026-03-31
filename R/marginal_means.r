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
#' @param data A data frame containing the conjoint data
#' @param formula A formula of the form `outcome ~ attr1 + attr2 + ...`
#' @param outcome (Alternative to `formula`) Character string naming the outcome variable
#' @param attributes (Alternative to `formula`) Character vector of attribute names
#' @param id (Optional) A one-sided formula specifying the ID/cluster variable, e.g. `~id`
#' @param wts (Optional) Weights to be used in the regression, as a one-sided formula
#'   (e.g. \code{~weights}).
#' @param design (Optional) A \code{survey::svydesign}-object. If a \code{design} is 
#'   provided, \code{cjlm} disregards \code{id} and \code{wts}, and adjusts
#'   based on the provided design instead.
#'
#' @return A data frame of class `marginal_means`
#'
#' @seealso \code{\link{amce}}, \code{\link{conditional_marginal_means}}
#'
#' @examples
#' library(conjest)
#' data("immigration")
#'
#' immigration |> marginal_means(ChosenImmigrant ~ Gender + Education, id = ~CaseID)
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
    mod$level     <- levels(data[[attr]])
    
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
#' @param data A data frame containing the conjoint data
#' @param formula A formula of the form `outcome ~ attr1 + attr2 + ...`
#' @param outcome (Alternative to `formula`) Character string naming the outcome variable
#' @param attributes (Alternative to `formula`) Character vector of attribute names
#' @param id (Optional) A one-sided formula specifying the ID/cluster variable, e.g. `~id`
#' @param group The respondent-level grouping variable (unquoted). Marginal
#'   means are estimated separately for each level of this variable.
#' @param wts (Optional) Weights to be used in the regression, as a one-sided
#'   formula (e.g. \code{~weights}).
#' @param design A \code{survey::svydesign}-object. If a \code{design} is provided,
#'   \code{id} and \code{weights} are ignored, and adjustments are made based
#'   on the provided design instead.
#'
#' @return A data frame of class `marginal_means`
#'
#' @seealso \code{\link{marginal_means}}, \code{\link{conditional_amce}}
#' 
#' @references Leeper, T. J., Hobolt, S. B., and Tilley, J. (2020). Measuring
#'   Subgroup Preferences in Conjoint Experiments. \emph{Political Analysis},
#'   28(2), 207--221. \doi{10.1017/pan.2019.30}
#'
#' @examples
#' ## Not run:
#' data |> conditional_marginal_means(selected ~ sex + age, group = resp_age, id = ~id)
#' @export
conditional_marginal_means <- function(data, formula = NULL, outcome = NULL, attributes = NULL, id = NULL, group = NULL, wts = NULL, design = NULL) {
  conditional_estimates(
    data, formula, outcome, attributes,
    groupvar   = rlang::as_name(rlang::ensym(group)),
    id         = id,
    wts        = wts,
    design     = design,
    .estimator = marginal_means,
    .class     = "conditional_marginal_means"
  )
}

#' @importFrom ggplot2 autoplot
#' @export
autoplot.marginal_means <- function(object, ...) {
  
  object$label <- paste0(object$attribute, ": ", object$level)
  
  object |> 
    ggplot2::ggplot(ggplot2::aes(x = estimate, y = label, color = attribute)) +
    ggplot2::geom_vline(xintercept = .5, lty = "dotted") +
    ggplot2::geom_point() +
    ggplot2::geom_linerange(ggplot2::aes(xmin = lower, xmax = upper)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Marginal Mean", y = "")
  
}

#' @importFrom ggplot2 autoplot
#' @export
autoplot.conditional_marginal_means <- function(object, ...) {
  
  group <- attr(object, "group")
  
  object |> 
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
summary.marginal_means <- function(object, ...) {
  
  attrs <- unique(object$attribute)
  
  cat("Marginal Means\n")
  cat(strrep("=", 60), "\n\n")
  
  purrr::walk(attrs, function(attr) {
    
    cat("Attribute:", attr, "\n")
    cat(strrep("-", 60), "\n")
    
    subset <- object[object$attribute == attr, ]
    
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
summary.conditional_marginal_means <- function(object, ...) {
  
  group_var <- attr(object, "group")
  attrs     <- unique(object$attribute)
  
  cat("Conditional Marginal Means\n")
  cat(strrep("=", 60), "\n\n")
  
  purrr::walk(attrs, function(attr) {
    
    cat("Attribute:", attr, "\n")
    cat(strrep("-", 60), "\n")
    
    attr_data <- object[as.character(object$attribute) == attr, ]
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
