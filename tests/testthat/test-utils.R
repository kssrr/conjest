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

test_that("assert_exist aborts when variable is missing", {
  df <- data.frame(x = 1:3, y = 1:3)
  expect_error(assert_exist(df, c("x", "z")), "do not exist")
  expect_no_error(assert_exist(df, c("x", "y")))
})

test_that("assert_exist aborts with the missing variable name", {
  df <- data.frame(x = 1:3)
  expect_error(assert_exist(df, c("x", "z", "w")), "z")
})

test_that("check_data drops rows with NAs and warns", {
  df <- data.frame(
    x = c(1, 2, NA, 4),
    y = c(1, NA, 3, 4)
  )
  expect_warning(result <- check_data(df, c("x", "y")), "Dropping")
  expect_equal(nrow(result), 2L)
})

test_that("check_data warns with correct row count", {
  df <- data.frame(x = c(1, NA, NA), y = c(1, 2, 3))
  expect_warning(check_data(df, "x"), "2 rows")
})

test_that("check_data does not warn when no NAs", {
  df <- data.frame(x = 1:3, y = 1:3)
  expect_no_warning(result <- check_data(df, c("x", "y")))
  expect_equal(nrow(result), 3L)
})

test_that("check_data only considers variables passed, not full data frame", {
  df <- data.frame(x = c(1, 2, 3), y = c(1, NA, 3))
  # NA is in y but we only check x, so no rows should be dropped
  expect_no_warning(result <- check_data(df, "x"))
  expect_equal(nrow(result), 3L)
})

# The following two check whether `check_data` and `assert_fct`
# correctly delegate to `assert_exist`

test_that("check_data aborts when variable does not exist", {
  df <- data.frame(x = 1:3)
  expect_error(check_data(df, c("x", "z")), "do not exist")
})

test_that("assert_fct aborts when variable does not exist", {
  df <- data.frame(x = factor(c("a", "b")))
  expect_error(assert_fct(df, c("x", "z")), "do not exist")
})
