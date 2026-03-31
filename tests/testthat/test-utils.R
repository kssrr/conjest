test_that("make_stars returns correct stars", {
  expect_equal(make_stars(0.0001), "***")
  expect_equal(make_stars(0.005),  "** ")
  expect_equal(make_stars(0.03),   "*"  )
  expect_equal(make_stars(0.07),   "."  )
  expect_equal(make_stars(0.5),    ""   )
})

test_that("make_stars handles NA", {
  expect_equal(make_stars(NA_real_), NA_character_)
})

test_that("format_number uses scientific notation for small numbers", {
  expect_match(format_number(1e-5), "e")
  expect_no_match(format_number(0.5), "e")
})

test_that("assert_fct throws on non-factor", {
  df <- data.frame(x = c("a", "b"), y = factor(c("a", "b")))
  expect_error(assert_fct(df, c("x", "y")), "factor")
  expect_no_error(assert_fct(df, "y"))
})
