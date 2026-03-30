# These are the functions that do most of the work in the background, along some
# smaller helpers, validators, etc.

# The user can pass design elements (clustering/ID variable, weights) either 
# directly, or via `survey::svydesign`. This function makes sure that we always
# end up with a valid design object to give to `survey::svyglm`, which does the
# actual estimation.
#
#' @noRd
validate_design <- function(data, design, id, wts) {
  
  if (!is.null(design)) {
    
    if (!inherits(design, "survey.design")) {
      cli::cli_abort(c(
        "{.arg design} must be a {.cls survey.design} object.",
        "i" = "Create one with {.fn survey::svydesign}."
      ), call = rlang::caller_env())
    }
    
    # warn if id/wts are also provided since they will be ignored
    
    ignored <- c(if (!is.null(id)) "id", if (!is.null(wts)) "wts")
    
    if (length(ignored) > 0) {
      cli::cli_inform(c(
        "{cli::qty(ignored)}{?This argument is/These arguments are} ignored when {.arg design} is provided: {.arg {ignored}}.",
        "i" = "Weights and clustering should be specified via the {.cls survey.design} object."
      ), call = rlang::caller_env())
    }
    
    return(design)
    
  }
  
  # no design & no id/clustering variable = probably unintended, warn the user
  
  if (is.null(id)) {
    cli::cli_warn(c(
      "No clustering variable provided.",
      "i" = "Conjoint experiments typically have repeated observations per respondent.",
      "i" = "Consider providing {.arg id} to obtain cluster-robust standard errors."
    ), call = rlang::caller_env())
  }
  
  survey::svydesign(
    ids = if (!is.null(id)) id else ~1,
    weights = wts,
    data = data
  )
  
}

# Check if all provided attribute are factors, throw if not (used for AMCE
# and Marginal Means to ensure coherent behavior).
#
#' @noRd
assert_fct <- function(data, attributes) {
  
  non_factors <- attributes[!sapply(attributes, function(x) is.factor(data[[x]]))]
  
  if (length(non_factors) > 0) {
    cli::cli_abort(c(
      "All attributes should be `factor` variables.",
      "x" = "Non-factor {cli::qty(non_factors)}attribute{?s}: {.var {non_factors}}"
    ), call = rlang::caller_env())
  }
  
}

# Get stars for significance levels for printouts. Can be called like
# `p_values |> make_stars(c(.01 = "***", .05 = "**", .1 = "*"))`
#
#' @noRd
make_stars <- function(p, thresholds = c(.001, .01, .05, .1), labels = c("***", "** ", "*", ".", "")) {
  
  if (!is.null(names(thresholds))) {
    labels     <- c(thresholds, "   ")
    thresholds <- as.numeric(names(thresholds))
  }
  
  labels[findInterval(p, c(thresholds, 1)) + 1]
  
}

# "Pretty" number formatting; use scientific notation if numbers get too large
# or small.
#
#' @noRd
format_number <- function(x, thres = 1e-4) {
  ifelse(
    abs(x) < thres,
    formatC(x, format = "e", digits = 2),
    formatC(x, format = "f", digits = 4)
  )
}

# `cjlm` is basically the backend that fits the actual model and that `amce` & 
# `marginal_means` call into. It just wraps `survey::svyglm` which is used here
# to estimate a linear model adjusting for design features.
#
#' Fit a Custom Linear Model for Conjoint Data
#'
#' Fit arbitrary linear models to conjoint data. Unlike \code{\link{amce}}
#' and \code{\link{marginal_means}}, which handle formula parsing, baseline
#' reconstruction, and result formatting automatically, \code{cjlm} is intended
#' for custom model specifications that do not fit the other functions in this package, 
#' such as models with interactions, continuous predictors, or
#' respondent-level covariates. The result is a tidy data frame of coefficients
#' that the user can process and plot as needed.
#'
#' @param data A data frame containing the conjoint data.
#' @param formula A formula specifying the model, e.g.
#'   \code{outcome ~ A + B + A:B} or \code{outcome ~ A * B + covariate}.
#' @param id (Optional) A one-sided formula specifying the clustering variable for
#'   cluster-robust standard errors, e.g. \code{~uuid}.
#' @param wts (Optional) Weights to be used in the regression as one-sided formula,
#'   e.g. \code{~weights}.
#' @param design (Optional) A \code{survey::svydesign}-object. If a `design` is
#'   passed, `id` and `wts` are ignored (they should be passed via the design).
#'
#' @return A tidy data frame of class \code{cjlm} with columns \code{term},
#'   \code{estimate}, \code{std.error}, \code{statistic}, and \code{p.value},
#'   as returned by \code{\link[broom]{tidy}}. The model object is stored as an 
#'   attribute of the result and can be retrieved via \code{attr(result, "model")}
#'   if needed.
#'
#' @seealso \code{\link{amce}}, \code{\link{marginal_means}},
#'   \code{\link{conditional_amce}}, \code{\link{conditional_marginal_means}}
#'
#' @examples
#' # Custom interaction model not covered by amce()
#' data("immigration")
#' 
#' cjlm(immigration, ChosenImmigrant ~ Education * Gender, id = ~CaseID)
#' 
#' # The actual model can also be retrieved from the result:
#' 
#' res <- immigration |> cjlm(ChosenImmigrant ~ Education * Gender, id = ~CaseID)
#' res |> attr("model")
#' 
#' # Custom methods:
#' summary(res)
#' print(res)
#' ggplot2::autoplot(res)
#'
#' @export
cjlm <- function(data, formula = NULL, id = NULL, wts = NULL, design = NULL) {
  
  design <- validate_design(data, design, id, wts)

  model <- survey::svyglm(formula, design, family = gaussian())
  res <- broom::tidy(model)
  
  class(res) <- c("cjlm", class(res))
  attr(res, "id") <- id
  attr(res, "model") <- model
  
  res
  
}

#' @export
summary.cjlm <- function(df, ...) {
  
  # add stars for significance levels & format numbers
  
  numeric_cols     <- c("estimate", "std.error", "statistic", "p.value")
  df$stars         <- make_stars(df$p.value)
  df[numeric_cols] <- lapply(df[numeric_cols], format_number)
  
  # Printing:
  
  cat("\nConjoint Analysis (Linear Model)\n\n")
  
  out <- data.frame(
    ` `          = df$term,
    `Estimate`   = df$estimate,
    `Std. Error` = df$std.error,
    `t value`    = df$statistic,
    `Pr(>|t|)`   = df$p.value,
    ` `          = df$stars,
    check.names  = FALSE
  )
  
  print(out, row.names = FALSE)
  cat("\n")
  cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  
}

# Apply some estimator separately by groups. Used to implement subgroup analysis
# via conditional AMCEs and conditional marginal means.
#
#' @noRd
conditional_estimates <- function(data, formula, outcome, attributes, groupvar, wts, design, .estimator, .class) {
  
  full_design <- validate_design(data, design, id = NULL, wts = wts)
  groups      <- unique(data[[groupvar]])
  
  result <- lapply(groups, function(g) {
    
    sub_design <- full_design[full_design$variables[[groupvar]] == g, ]
    
    sub_res <- .estimator(
      data,
      formula    = formula,
      outcome    = outcome,
      attributes = attributes,
      design     = sub_design
    )
    
    sub_res[[groupvar]] <- g
    sub_res
    
  }) |> do.call(rbind, args = _)
  
  class(result) <- c(.class, class(result))
  attr(result, "group") <- groupvar
  
  result
  
}