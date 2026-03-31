data("immigration", package = "conjest")

test_that("conditional_amce returns correct class", {
  result <- conditional_amce(immigration, ChosenImmigrant ~ Education, id = ~CaseID, group = Gender)
  expect_s3_class(result, "conditional_amce")
})

test_that("conditional_amce group attribute is set correctly", {
  result <- conditional_amce(immigration, ChosenImmigrant ~ Education, id = ~CaseID, group = Gender)
  expect_equal(attr(result, "group"), "Gender")
})

test_that("conditional_amce has one set of results per group level", {
  result    <- conditional_amce(immigration, ChosenImmigrant ~ Education, id = ~CaseID, group = Gender)
  amce_base <- amce(immigration, ChosenImmigrant ~ Education, id = ~CaseID)
  expect_equal(nrow(result), nrow(amce_base) * length(levels(immigration$Gender)))
})

test_that("conditional_marginal_means returns correct class", {
  result <- conditional_marginal_means(immigration, ChosenImmigrant ~ Education, id = ~CaseID, group = Gender)
  expect_s3_class(result, "conditional_marginal_means")
})

test_that("conditional estimates differ across groups", {
  result <- conditional_marginal_means(immigration, ChosenImmigrant ~ Education, id = ~CaseID, group = Gender)
  male   <- result[result$Gender == "Male",   "estimate"]
  female <- result[result$Gender == "Female", "estimate"]
  expect_false(isTRUE(all.equal(male, female)))
})
