#' @importFrom stats terms reformulate gaussian complete.cases pnorm anova as.formula lm
#' @importFrom ggplot2 autoplot
NULL

# In some methods, we rely intentionally on unbound names that will be defined
# at run time. Declare them ahead as global variables to tell the checks that this
# is intentional.
utils::globalVariables(c(
  "estimate", "level", "attribute", "sig",
  "lower", "upper", "label", ".data"
))