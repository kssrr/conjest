#' Nested model comparisons for conjoint attribute heterogeneity
#'
#' Tests whether the effect of each attribute in a conjoint experiment varies
#' across subgroups via nested model comparison, similar to what
#' Leeper et al. (2020) recommend. For each attribute, a
#' restricted model (attribute only) is compared against an unrestricted model
#' (attribute × subgroup interaction). The test used depends on the arguments
#' supplied:
#'
#' \itemize{
#'   \item **Weights** (`wts`): fits both models via [survey::svyglm()] and
#'     compares them with a Rao-Scott likelihood-ratio test
#'     ([survey::anova.svyglm()]).
#'   \item **Cluster ID, no weights** (`id` only): fits both models via
#'     [stats::lm()] and compares them with a cluster-robust Wald test
#'     ([lmtest::waldtest()]) using [sandwich::vcovCL()] for the
#'     variance–covariance matrix.
#'   \item **Neither**: fits both models via [stats::lm()] and compares them
#'     with a standard F-test ([stats::anova()]). A warning is issued because
#'     conjoint data typically has a clustering structure.
#' }
#' 
#' @param data A data frame containing all variables referenced in `formula`
#'   and `by`.
#' @param formula A two-sided formula of the form `outcome ~ attr1 + attr2 +
#'   ...`. The left-hand side is the outcome variable; the right-hand side
#'   lists the conjoint attributes to test.
#' @param by A character string naming the subgroup variable in `data`. Each
#'   attribute is tested for heterogeneity across the levels of this variable.
#' @param id A one-sided formula identifying the clustering unit (e.g.
#'   `~respondent_id`). When supplied without `wts`, a cluster-robust Wald
#'   test is used. When supplied together with `wts`, it is passed to
#'   [survey::svydesign()] as the `ids` argument. Defaults to `NULL`.
#' @param wts A one-sided formula identifying the survey-weight variable (e.g.
#'   `~weight`). When supplied, models are fitted via [survey::svyglm()] and
#'   compared with a Rao-Scott LR-test. Defaults to `NULL`.
#'
#' @return A named list with one element per attribute on the right-hand side
#'   of `formula`. Each element is the test object returned by the underlying
#'   comparison function:
#'   \itemize{
#'     \item an `anova.svyglm` object when `wts` is supplied;
#'     \item a `waldtest` object when only `id` is supplied;
#'     \item an `anova` object when neither is supplied.
#'   }
#'
#' @seealso [survey::svyglm()], [lmtest::waldtest()], [sandwich::vcovCL()],
#'   [stats::anova()]
#'
#' @references Leeper, T. J., Hobolt, S. B., and Tilley, J. (2020). Measuring
#'   Subgroup Preferences in Conjoint Experiments. \emph{Political Analysis},
#'   28(2), 207--221. \doi{10.1017/pan.2019.30}
#'
#' @examples
#' 
#' library(conjest)
#' data("trust")
#' 
#' # Cluster-robust Wald test (typical conjoint use case)
#' nested_model_comp(
#'   data    = trust,
#'   formula = selected ~ group + age + class,
#'   by      = "resp_sex",
#'   id      = ~uuid
#' )
#'
#' # Rao-Scott LR-test with survey weights
#' nested_model_comp(
#'   data    = trust,
#'   formula = selected ~ group + age + class,
#'   by      = "resp_sex",
#'   id      = ~uuid,
#'   wts     = ~weight
#' )
#' 
#' @export
nested_model_comp <- function(data, formula, by, id = NULL, wts = NULL) {
  
  y     <- deparse(rlang::f_lhs(formula))
  attrs <- labels(terms(formula))
  
  data <- check_data(data, c(all.vars(formula), by))
  
  # weights provided -> use `survey::svyglm` + `stats::anova`
  # (`survey` provides a method for anova that conducts a 
  # proper Rao-Scott LR-Test)
  
  if (!is.null(wts)) {
    
    des <- validate_design(data, design = NULL, id, wts)
    
    # `anova.svyglm()` has an internal `update()`-call that calls
    # `svyglm()` unqualified (not as `survey::svyglm()`), leading to
    # a confusing error when the `survey`-package itself is not loaded.
    # Exposing `svyglm` unqualified so this does not happen:
    svyglm <- survey::svyglm
    
    res <- lapply(attrs, function(attr) {
      
      fml_res   <- reformulate(termlabels = attr, response = y)
      fml_unres <- as.formula(paste(y, "~", attr, "*", by))
      
      anova(
        svyglm(fml_res, design = des),
        svyglm(fml_unres, design = des)
      )
      
    })
    
    names(res) <- attrs
    return(res)
    
  }
  
  # cluster ID but no weights -> cluster-robust Wald test
  
  if (!is.null(id)) {
    
    res <- lapply(attrs, function(attr) {
      
      fml_res   <- reformulate(termlabels = attr, response = y)
      fml_unres <- as.formula(paste(y, "~", attr, "*", by))
      
      res   <- lm(fml_res, data = data)
      unres <- lm(fml_unres, data = data)
      
      lmtest::waldtest(res, unres, vcov = sandwich::vcovCL(unres, cluster = id))
    })
    
    names(res) <- attrs
    return(res)
    
  }
  
  # No ID & no weights -> `stats::lm` + F-test (via `stats::anova`),
  # also warn the user that their data probably has clustering structure:
  
  cli::cli_warn(c(
    "No clustering variable provided.",
    "i" = "Conjoint experiments typically have repeated observations per respondent.",
    "i" = "Consider providing {.arg id} to obtain results that adjust for clustering."
  ), call = rlang::caller_env())
  
  res <- lapply(attrs, function(attr) {
    
    fml_res   <- reformulate(termlabels = attr, response = y)
    fml_unres <- as.formula(paste(y, "~", attr, "*", by))
    
    anova(
      lm(fml_res, data = data),
      lm(fml_unres, data = data),
      test = "F"
    )
    
  })
  
  names(res) <- attrs
  res
}
