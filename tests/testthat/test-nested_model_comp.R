# ── Dispatch: cluster-robust Wald test ────────────────────────────────────────

test_that("clustered path returns anova objects (lmtest::waldtest)", {
  result <- nested_model_comp(trust, selected ~ group + age, by = "resp_sex", id = ~uuid)
  expect_true(all(vapply(result, inherits, logical(1), "anova")))
})

test_that("clustered path does not warn about missing clustering variable", {
  expect_no_warning(
    nested_model_comp(trust, selected ~ group + age, by = "resp_sex", id = ~uuid)
  )
})

test_that("clustered path p-values are in [0, 1]", {
  result <- nested_model_comp(trust, selected ~ group + age, by = "resp_sex", id = ~uuid)
  p_vals <- vapply(result, function(x) x[2, "Pr(>F)"], numeric(1))
  expect_true(all(p_vals >= 0 & p_vals <= 1))
})


# ── Dispatch: survey-weighted Rao-Scott LR test ───────────────────────────────

test_that("weighted path returns regTermTest objects", {
  result <- nested_model_comp(trust, selected ~ group + age, by = "resp_sex", id = ~uuid, wts = ~weight)
  expect_true(all(vapply(result, inherits, logical(1), "regTermTestLRT")))
})

test_that("weighted path does not warn about missing clustering variable", {
  expect_no_warning(
    nested_model_comp(trust, selected ~ group + age, by = "resp_sex", id = ~uuid, wts = ~weight)
  )
})


# ── Edge cases & bad inputs ───────────────────────────────────────────────────

test_that("wts without id warns about missing clustering variable", {
  expect_warning(
    nested_model_comp(trust, selected ~ group, by = "resp_sex", wts = ~weight),
    "clustering"
  )
})