# Calling handler used everywhere to throw consistent errors
#
#' @noRd

# The user can pass design elements (clustering/ID variable, weights) either 
# directly, or via `survey::svydesign`. 
#
#' @noRd
check_design <- function(design, id, wts) {
  
  # we got a design:
  
  if (!is.null(design)) {
    
    if (!inherits(design, "survey.design")) {
      
      cli::cli_abort(c(
        "{.arg design} must be a {.cls survey.design} object.",
        "i" = "Create one with {.fn survey::svydesign}."
      ))
      
    }
    
    # if we also got IDs and/or weights, use the design, drop the other args
    # & tell the user about it:
    
    if (!is.null(id) || !is.null(wts)) {
      
      ignored <- c(if (!is.null(id))  "id", if (!is.null(wts)) "wts")
      
      cli::cli_warn(c(
        "{.arg design} is provided alongside {cli::qty(ignored)}{?an/} ignored {cli::qty(ignored)}argument{?s}.",
        "i" = "{cli::qty(ignored)}{?This argument is/These arguments are} ignored when {.arg design} is provided: {.arg {ignored}}.",
        "i" = "Weights and/or clustering should be specified via the {.cls survey.design} object instead if one is provided."
      ))
      
    }
    
  }
  
  # if we get no design AND no ID var, warn the user about it as this is 
  # probably unintended
  
  if (is.null(id) && is.null(design)) {
    
    cli::cli_warn(c(
      "No clustering variable provided.",
      "i" = "Conjoint experiments typically have repeated observations per respondent.",
      "i" = "Consider providing {.arg id} to obtain cluster-robust standard errors."
    ), call = rlang::caller_env())
    
  }
  
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

# This makes the `wts`-argument more flexible than default `lm`, which takes
# `NULL` or a numeric vector; we also allow column names, either as strings or
# unquoted names:
#
#' @noRd
parse_wts <- function(data, wts, wts_expr) {
  
  # if we get `NULL` or a numeric vector that is fine:
  
  if (is.null(wts) || is.numeric(wts))
    return(wts)
  
  # if we get a string or an unquoted name, try to interpret it as a column
  # inside `data`
  
  col_name <- if (is.character(wts)) wts else deparse(wts_expr)
  
  if (!col_name %in% names(data))
    cli::cli_abort("Column {.var {col_name}} not found in {.arg data}.")
  
  data[[col_name]]
  
}

# Utility functions to fit generic model for conjoint analysis
#
# This is a pretty generic wrapper around `lm` and `lmtest`/`sandwich` to fit a 
# linear model with clustered standard errors, and return a tidy data frame.

#' Fit a Custom Linear Model for Conjoint Data
#'
#' A flexible low-level function for fitting arbitrary linear models to conjoint
#' data with optional cluster-robust standard errors. Unlike \code{\link{amce}}
#' and \code{\link{marginal_means}} — which handle formula parsing, baseline
#' reconstruction, and result formatting automatically — \code{cjlm} is intended
#' for custom model specifications that do not fit the other functions in this package, 
#' such as models with interactions, continuous predictors, or
#' respondent-level covariates. The result is a tidy data frame of coefficients
#' that the user can process and plot as needed.
#'
#' @param data A data frame containing the conjoint data.
#' @param formula A formula specifying the model, e.g.
#'   \code{outcome ~ A + B + A:B} or \code{outcome ~ A * B + covariate}.
#' @param id (Optional) A one-sided formula specifying the clustering variable for
#'   cluster-robust standard errors, e.g. \code{~uuid}. If \code{NULL},
#'   standard OLS standard errors are used and a warning is issued.
#' @param vcov_type The type of heteroskedasticity-consistent covariance
#'   estimator passed to \code{\link[sandwich]{vcovCL}}. Defaults to
#'   \code{"HC1"}, allows HC0-HC3.
#' @param wts (Optional) Weights to be used in the regression. Can be
#'   \code{NULL} (the default), a numeric vector, or the name of a 
#'   column in \code{data} (quoted or unquoted).
#' @param design A \code{survey::svydesign}-object. If a \code{design} is 
#'   provided, \code{cjlm} uses \code{survey::svyglm} as backend using 
#'   the provided design, disregarding other arguments like \code{id},
#'   \code{vcov_type}, and \code{wts}, as all of these are handled
#'   by \code{survey::svyglm} (see \code{?survey::svyglm}).
#'
#' @return A tidy data frame of class \code{cjlm} with columns \code{term},
#'   \code{estimate}, \code{std.error}, \code{statistic}, and \code{p.value},
#'   as returned by \code{\link[broom]{tidy}}. The clustering variable is
#'   stored as an attribute on the result.
#'
#' @seealso \code{\link{amce}}, \code{\link{marginal_means}},
#'   \code{\link{conditional_amce}}, \code{\link{conditional_marginal_means}}
#'
#' @examples
#' # Custom interaction model not covered by amce()
#' cjlm(data, ChosenImmigrant ~ Education * Gender, id = ~CaseID)
#'
#' # Model with a respondent-level covariate
#' cjlm(data, ChosenImmigrant ~ Education + resp_age, id = ~CaseID)
#'
#' @export
cjlm <- function(data, formula = NULL, id = NULL, vcov_type = "HC1", wts = NULL, design = NULL) {
  
  # try to parse whatever was provided as weights:
  weights <- parse_wts(data, wts, substitute(wts))
  
  # see what we got as a design & whether it makes sense:
  check_design(design, id, wts)

  
  if (is.null(design))
    design <- survey::svydesign(id = id, weight = wts, data = data)
  
  model <- survey::svyglm(formula, design) 
  res <- broom::tidy(model)
  
  class(res) <- c("cjlm", class(res))
  attr(res, "id") <- id
  attr(res, "model") <- model
  
  res
  
}

#' @export
summary.cjlm <- function(df, ...) {
  
  df <- 
    df |> 
    dplyr::mutate(
      stars = dplyr::case_when(
        p.value < .001 ~ "***",
        p.value < .01 ~ "**",
        p.value < .05 ~ "*",
        p.value < .1 ~ ".",
        TRUE ~ ""
      ),
      dplyr::across(estimate:p.value, function(x) {
        
        ifelse(
          abs(x) < 1e-4,
          formatC(x, format = "e", digits = 2),
          formatC(x, format = "f", digits = 4)
        )
        
      })
    )
  
  # Printout:
  
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
  
  id <- attr(df, "id")
  
  if (!is.null(id)) {
    
    cat(paste(
      "Standard errors clustered by:",
      paste(all.vars(rlang::f_rhs(id)), collapse = ", "),
      "\n"
    ))
    
  }
  
  cat("Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  cat(paste("Backend:", attr(df, "backend")))
  
}
