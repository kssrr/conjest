data("trust", package = "conjest")

test_that("marginal_means returns correct class and structure", {
  result <- mm(trust, selected ~ education + sex, id = ~uuid)
  expect_s3_class(result, "marginal_means")
  expect_named(result, c("attribute", "level", "term", "estimate", "std.error", "lower", "upper", "z", "p"))
})

test_that("marginal_means estimates are bounded between 0 and 1", {
  result <- mm(trust, selected ~ education + sex, id = ~uuid)
  expect_true(all(result$estimate >= 0 & result$estimate <= 1))
})

test_that("marginal means returns correct number of rows (one for each level of each attribute)", {
  result <- mm(trust, selected ~ education + sex, id = ~uuid)
  expect_equal(nrow(result), length(levels(trust$education)) + length(levels(trust$sex)))
})
