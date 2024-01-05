# validate() tests -------------------------------------------------------------


test_that("validate_input_df function works correctly", {
  expect_error(validate_input_df(123),
               class = "generalbfxpkg_error_data",
               info = list(class_data = "numeric"))
  expect_error(validate_input_df("not a data frame"),
               class = "generalbfxpkg_error_data",
               info = list(class_data = "character"))
  expect_invisible(validate_input_df(iris))
})

test_that("validate_input_cols function works correctly", {
  df <- data.frame(A = 1:5, B = 6:10)
  expect_error(validate_input_cols(df, c("A", "C")),
               class = "generalbfxpkg_error_cols",
               info = list(cols_req = c("A", "C"), cols_data = c("A", "B")))
  expect_invisible(validate_input_cols(df, c("A", "B")))
})

test_that("validate_uncommon_visits function works correctly", {
  df <- data.frame(
    'VISIT' = c('Scheduled', 'Unscheduled', 'Screening'),
    'Other' = c(1, 2, 3)
  )

  # Test case with default err.visits and visit.col
  expect_error(validate_uncommon_visits(df),
               class = "generalbfxpkg_error_visit",
               info = list(uniq_visits = tolower(unique(df[['VISIT']])),
                           show_err_visits = c("unscheduled", "screening")))

  # Test case with additional err.visits specified
  expect_error(validate_uncommon_visits(df, err.visits = c("Scheduled")),
               class = "generalbfxpkg_error_visit",
               info = list(uniq_visits = tolower(unique(df[['VISIT']])),
                           show_err_visits = c("scheduled", "unscheduled", "screening")))

  # Test case where there are no errant visits
  df_good <- data.frame(
    'VISIT' = c('Scheduled', 'Another Scheduled Visit', 'Yet Another Scheduled Visit'),
    'Other' = c(1, 2, 3)
  )
  expect_message(validate_uncommon_visits(df_good), "No issues with columns. Proceed!")
})
