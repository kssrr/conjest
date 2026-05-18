data("trust", package = "conjest")

test_that("conditional_amce returns correct class", {
  result <- conditional_amce(trust, selected ~ education, id = ~uuid, group = resp_sex)
  expect_s3_class(result, "conditional_amce")
})

test_that("conditional_amce group attribute is set correctly", {
  result <- conditional_amce(trust, selected ~ education, id = ~uuid, group = resp_sex)
  expect_equal(attr(result, "group"), "resp_sex")
})

test_that("conditional_amce has one set of results per group level", {
  result    <- conditional_amce(trust, selected ~ education, id = ~uuid, group = resp_sex)
  amce_base <- amce(trust, selected ~ education, id = ~uuid)
  expect_equal(nrow(result), nrow(amce_base) * length(levels(trust$resp_sex)))
})

test_that("conditional_marginal_means returns correct class", {
  result <- conditional_mm(trust, selected ~ education, id = ~uuid, group = resp_sex)
  expect_s3_class(result, "conditional_marginal_means")
})

test_that("conditional estimates differ across groups", {
  result <- conditional_mm(trust, selected ~ group, id = ~uuid, group = sex)
  male   <- result[result$sex == "Male",   "estimate"]
  female <- result[result$sex == "Female", "estimate"]
  expect_false(isTRUE(all.equal(male, female)))
})
