# Marginal means, conditional marginal means, and autoplot-methods for marginal means.

#' @export
marginal_means <- function(
  data, 
  formula = NULL, 
  outcome = NULL, 
  attributes = NULL, 
  cluster
) {
  
  if (!is.null(formula)) {
    
    outcome <- deparse(rlang::f_lhs(formula))
    attributes <- labels(terms(formula))
    
  }
  
  mods <- attributes |> lapply(function(x) {
    lm(reformulate(x, response = outcome, intercept = FALSE), data = data)
  })
  
  mods |>
    lapply(function(model) {
      
      model |>
        coeftest(vcov. = vcovCL(model, cluster = cluster, type = "HC1")) |>
        broom::tidy()
      
    }) |>
    do.call(rbind, args = _) |> 
    _[, c("term", "estimate", "std.error")]
  
}

#' @export
conditional_marginal_means <- function(df, formula, group, cluster) {
  
  df |> 
    dplyr::group_by({{ group }}) |> 
    tidyr::nest() |> 
    dplyr::mutate(
      mms = lapply(data, function(d) {
        
        marginal_means(
          d,
          formula,
          cluster = cluster
        )
        
      })
    ) |> 
    dplyr::select({{ group }}, mms) |> 
    tidyr::unnest(mms) |> 
    dplyr::ungroup()
  
}

