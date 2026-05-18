

nested_model_comp <- function(data, formula, by, id = NULL, wts = NULL) {
  
  y     <- deparse(rlang::f_lhs(formula))
  attrs <- labels(terms(formula))
  
  data <- check_data(data, c(all.vars(formula), by))
  
  # weights provided -> use `survey::svyglm` + `stats::anova`
  # (`survey` provides a method for anova that conducts a 
  # proper test)
  
  if (!is.null(wts)) {
    
    des <- validate_design(data, design = NULL, id, wts)
    
    res <- lapply(attrs, function(attr) {
      
      fml_res   <- reformulate(termlabels = attr, response = y)
      fml_unres <- as.formula(paste(y, "~", attr, "*", by))
      
      anova(
        survey::svyglm(fml_res, design = des),
        survey::svyglm(fml_unres, design = des)
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
