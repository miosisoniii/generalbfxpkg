# TESTS FOR shap_w() -----------------------------------------------------------

test_that("shap_w correctly applies Shapiro-Wilk test when conditions are met", {
  df <- data.frame(RESULT = rnorm(100))
  p_value <- shap_w(df, "RESULT")
  expect_true(is.numeric(p_value))
})

test_that("shap_w returns NA when conditions are not met", {
  df <- data.frame(RESULT = rep(1, 100))
  p_value <- shap_w(df, "RESULT")
  expect_true(is.na(p_value))
})

test_that("shap_w throws an error when provided column name does not exist", {
  df <- data.frame(RESULT = rnorm(100))
  expect_error(shap_w(df, "NON_EXISTING_COL"),
               "Data frame is missing some required columns")
})

test_that("shap_w returns error when provided data frame is NULL", {
  expect_error(shap_w(NULL, "RESULT"), "Must supply a data frame")
})

test_that("shap_w returns error when provided column name is NULL", {
  df <- data.frame(RESULT = rnorm(100))
  expect_error(shap_w(df, NULL), "attempt to select less than one element in get1index")
})

# tests for wilcox_test_grouped() ----------------------------------------------

test_that("wilcox_test_grouped does not throw an error when df and all colnames exist", {
  df <- data.frame(
    RESULT = c(rnorm(50), rnorm(50, mean = 3)),
    STIM = rep(c('group1', 'group2'), each = 50),
    PARAM = rep(c('A', 'B'), each = 25, times = 2),
    VISIT = rep(1:2, each = 25, times = 2)
  )
  expect_silent(
    wilcox_test_grouped(df, group1 = 'group1', group2 = 'group2')
    )
})

test_that("wilcox_test_grouped throws an error when df does not exist", {
  expect_error(
    wilcox_test_grouped(NULL),
    "Must supply a data frame"
    )
})

test_that("wilcox_test_grouped throws an error when provided column names do not exist", {
  df <- data.frame(RESULT = rnorm(100),
                   STIM = rep(c('ConA', 'HPV16'), 50),
                   PARAM = rep(c('A', 'B'), 50),
                   VISIT = rep(1:2, 50))
  expect_error(
    wilcox_test_grouped(
      df,
      result.col = "NON_EXISTING_COL",
      grouping.factor = "NON_EXISTING_COL"),
    "Data frame is missing some required columns"
    )
})

test_that("wilcox_test_grouped returns error when provided data frame is NULL", {
  expect_error(
    wilcox_test_grouped(NULL, "RESULT", "STIM"),
    "Must supply a data frame"
    )
})

test_that("wilcox_test_grouped returns error when provided column names are NULL", {
  df <- data.frame(RESULT = rnorm(100),
                   STIM = rep(c('group1', 'group2'), each = 50),
                   PARAM = rep(c('A', 'B'), each = 50),
                   VISIT = rep(1:2, 50))

  expect_error(
    wilcox_test_grouped(df, c("PARAM", "VISIT"), NULL, NULL),
    "Error: Column names cannot be NULL.")
})
