data("immigration", package = "conjest")

test_that("amce returns correct class and structure", {
  result <- amce(immigration, ChosenImmigrant ~ Education + Gender, id = ~CaseID)
  expect_s3_class(result, "amce")
  expect_named(result, c("attribute", "level", "term", "estimate", "std.error", "lower", "upper", "statistic", "p.value"))
})

test_that("amce baseline rows have estimate of zero", {
  result <- amce(immigration, ChosenImmigrant ~ Education + Gender, id = ~CaseID)
  baselines <- result[is.na(result$p.value), ]
  expect_true(all(baselines$estimate == 0))
})

test_that("amce has one baseline row per attribute", {
  result <- amce(immigration, ChosenImmigrant ~ Education + Gender, id = ~CaseID)
  n_baselines <- sum(is.na(result$p.value))
  expect_equal(n_baselines, 2)  # Education and Gender
})

test_that("amce returns correct number of rows", {
  result <- amce(immigration, ChosenImmigrant ~ Education + Gender, id = ~CaseID)
  n_levels <- length(levels(immigration$Education)) + length(levels(immigration$Gender))
  expect_equal(nrow(result), n_levels)
})

# this is just another way of testing the above & making sure
# amce & mm do the same thing (it kind of tests both amces and mms):

test_that("marginal_means and amce give same number of rows", {
  amce_result <- amce(immigration, ChosenImmigrant ~ Gender, id = ~CaseID)
  mm_result   <- marginal_means(immigration, ChosenImmigrant ~ Gender, id = ~CaseID)
  expect_equal(nrow(mm_result), nrow(amce_result))  # amce has one extra baseline row
})

test_that("amce throws on non-factor attributes", {
  immigration$Education <- as.character(immigration$Education)
  expect_error(
    amce(immigration, ChosenImmigrant ~ Education + Gender, id = ~CaseID),
    "factor"
  )
})

test_that("amce formula and outcome/attributes interfaces give same result", {
  r1 <- amce(immigration, ChosenImmigrant ~ Gender, id = ~CaseID)
  r2 <- amce(immigration, outcome = "ChosenImmigrant", attributes = "Gender", id = ~CaseID)
  expect_equal(r1$estimate, r2$estimate)
})

test_that("amce warns when no id provided", {
  expect_warning(
    amce(immigration, ChosenImmigrant ~ Gender),
    "clustering"
  )
})
