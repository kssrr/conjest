data("trust", package = "conjest")

test_that("amce returns correct class and structure", {
  result <- amce(trust, selected ~ education + sex, id = ~uuid)
  expect_s3_class(result, "amce")
  expect_named(result, c("attribute", "level", "term", "estimate", "std.error", "lower", "upper", "statistic", "p.value"))
})

test_that("amce baseline rows have estimate of zero", {
  result <- amce(trust, selected ~ education + sex, id = ~uuid)
  baselines <- result[is.na(result$p.value), ]
  expect_true(all(baselines$estimate == 0))
})

test_that("amce has one baseline row per attribute", {
  result <- amce(trust, selected ~ education + sex, id = ~uuid)
  n_baselines <- sum(is.na(result$p.value))
  expect_equal(n_baselines, 2)  # Education and Gender
})

test_that("amce returns correct number of rows", {
  result <- amce(trust, selected ~ education + sex, id = ~uuid)
  n_levels <- length(levels(trust$education)) + length(levels(trust$sex))
  expect_equal(nrow(result), n_levels)
})

test_that("amce throws on non-factor attributes", {
  trust$education <- as.character(trust$education)
  expect_error(
    amce(trust, selected ~ education + sex, id = ~uuid),
    "factor"
  )
})

test_that("amce formula and outcome/attributes interfaces give same result", {
  r1 <- amce(trust, selected ~ sex, id = ~uuid)
  r2 <- amce(trust, outcome = "selected", attributes = "sex", id = ~uuid)
  expect_equal(r1$estimate, r2$estimate)
})

test_that("amce warns when no id provided", {
  expect_warning(
    amce(trust, selected ~ sex),
    "clustering"
  )
})
