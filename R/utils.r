#' @noRd
validate_inputs <- function(data, attributes, id) {
  
  # check if all attributes are factors, throw if not:
  
  non_factors <- attributes[!sapply(attributes, function(x) is.factor(data[[x]]))]
  
  if (length(non_factors) > 0) {
    
    cli::cli_abort(c(
      "All attributes should be `factor` variables.",
      "x" = "Non-factor {cli::qty(non_factors)}attribute{?s}: {.var {non_factors}}"
    ), call = rlang::caller_env())
    
  }
 
  # warn the user if no clustering variable is passed:
  
  if (is.null(id)) {
    
    cli::cli_warn(c(
      "No clustering variable provided.",
      "i" = "Conjoint experiments typically have repeated observations per respondent.",
      "i" = "Consider providing {.arg id} to obtain cluster-robust standard errors."
    ), call = rlang::caller_env())
    
  } 
  
}