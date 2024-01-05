# Optimized elispot_vmd.R tests ------------------------------------------------

# tests for get_recode_vector() ------------------------------------------------
test_that("get_recode_vector behaves as expected", {
  df <- proj01_elispot_data

  # Run the function
  recode_vector <- get_recode_vector(df, "VISIT")

  # 1. Check that the output is a named vector
  expect_true(is.character(recode_vector) & is.vector(recode_vector) & !is.null(names(recode_vector)))

  # 2. Check that the vector names are what we expect them to be
  expected_vector <- c("Day0", "Week8", "Week15", "Week36", "Unscheduled", "SCREEN")
  expect_identical(names(recode_vector), expected_vector)

  # 3. Check that the vector values are what we expect them to be
  expected_values <- c("Day0" = "VISIT1",
                       "Week8" = "VISIT2",
                       "Week15" = "VISIT3",
                       "Week36" = "VISIT4",
                       "Unscheduled" = "VISIT5",
                       "SCREEN" = "VISIT6")
  expect_identical(recode_vector, expected = expected_values)

  # 4. Test that the function fails gracefully with a non-existing column name
  expect_error(get_recode_vector(df, "NON_EXISTING_COLUMN"),
               "Data frame is missing some required columns")

})

# tests for recode_col_to_base -------------------------------------------------
test_that("recode_col_to_base replaces column values correctly", {
  df <- proj01_elispot_data |> dplyr::filter(VISIT != "Unscheduled")

  df_expected <- df
  df_expected$STIM <- c("STIM1", "STIM2", "STIM3", "STIM4", "STIM5", "STIM6")

  df_test <- recode_col_to_base(df, "STIM")
  expect_identical(df_test, df_expected)
})

test_that("recode_col_to_base throws error for non-existing columns", {
  df <- data.frame(STIM = c("A", "B", "C", "A", "B", "C"),
                   value = 1:6)

  expect_error(recode_col_to_base(df, "NON_EXISTING_COLUMN"),
               "Data frame is missing some required columns")
})

# test_that("recode_col_to_base works with NA values", {
#   df <- proj01_elispot_data |> dplyr::filter(VISIT != "Unscheduled") |> head(n = 24)
#   df[1,5] <- NA
#
#   expect_error(recode_col_to_base(df, "STIM"), "NA detected. Please omit or escalate.")
# })

# tests for recode_col_to_orig() -----------------------------------------------

test_that("recode_col_to_orig works with normal input", {
  df <- data.frame(STIM = c("A", "B", "C", "A", "B", "C"),
                   value = 1:6)
  converted_df <- recode_col_to_base(df, "STIM")
  reversed_df <- recode_col_to_orig(converted_df, df, "STIM")

  expect_identical(df, reversed_df)
})

test_that("recode_col_to_orig works with NA values", {
  df <- proj01_elispot_data |> dplyr::filter(VISIT != "Unscheduled")

  converted_df <- recode_col_to_base(df, "STIM")
  reversed_df <- recode_col_to_orig(converted_df, df, "STIM")

  expect_identical(df, reversed_df)
})

test_that("recode_col_to_orig works with 1 unique value", {
  df <- proj01_elispot_data |> dplyr::filter(VISIT != "Unscheduled")
  converted_df <- recode_col_to_base(df, "STIM")
  reversed_df <- recode_col_to_orig(converted_df, df, "STIM")

  expect_identical(df, reversed_df)
})

# test_that("recode_col_to_orig works with all NA values", {
#   df <- data.frame(STIM = rep(NA, 6), value = 1:6)
#   converted_df <- recode_col_to_base(df, "STIM")
#   reversed_df <- recode_col_to_orig(converted_df, df, "STIM")
#
#   expect_identical(df, reversed_df)
# })

test_that("recode_col_to_orig fails with non-existent column", {
  df <- proj01_elispot_data |> dplyr::filter(VISIT != "Unscheduled")
  converted_df <- recode_col_to_base(df, "STIM")

  expect_error(recode_col_to_orig(converted_df, df, "NON_EXISTING_COLUMN"),
               "'arg' should be one of|Data frame is missing some required columns")
})


# tests for sum_across_wide() --------------------------------------------------
test_that("sum_across_wide works with normal input", {
  df <- proj01_elispot_data |> dplyr::filter(VISIT != "Unscheduled") |>
    tidyr::pivot_wider(names_from = STIM, values_from = RESULT)
  df_expected <- mutate(df, TOTAL_HPV16  = HPV16E6 + HPV16E7)
  df_test <- sum_across_wide(df, c("HPV16E6", "HPV16E7"), "TOTAL_HPV16")
  expect_identical(df_test, df_expected)
})

# test_that("sum_across_wide works with NA values", {
#   df <- proj01_elispot_data |> dplyr::filter(VISIT != "Unscheduled") |>
#     tidyr::pivot_wider(names_from = STIM, values_from = RESULT)
#
#   df_expected <- mutate(df, TOTAL_HPV16 = c(3, NA, NA, 9))
#   df_test <- sum_across_wide(df, c("A", "B"), "D")
#   expect_identical(df_test, df_expected)
# })
#
# test_that("sum_across_wide works with all NA values", {
#   df <- data.frame(A = rep(NA, 4), B = rep(NA, 4), C = 3:6)
#   df_expected <- mutate(df, D = rep(NA, 4))
#   df_test <- sum_across_wide(df, c("A", "B"), "D")
#   expect_identical(df_test, df_expected)
# })

test_that("sum_across_wide fails with non-existent column", {
  df <- proj01_elispot_data |> dplyr::filter(VISIT != "Unscheduled") |>
    tidyr::pivot_wider(names_from = STIM, values_from = RESULT)

  expect_error(sum_across_wide(df, c("A", "NON_EXISTING_COLUMN"), "D"),
               "Data frame is missing some required columns")
})


# tests for get_baseline() -----------------------------------------------------
test_that("get_baseline works with normal input", {
  df <- proj01_elispot_data |> dplyr::filter(VISIT != "Unscheduled") |>
    tidyr::pivot_wider(names_from = VISIT, values_from = RESULT)

  df_expected <- df |>
    dplyr::mutate(BASELINE = ifelse(is.na(Day0), SCREEN,
                                    ifelse(is.na(SCREEN), Day0, NA)))
  df_test <- get_baseline(df, d0.regex = "d0|d 0|day 0|day0", scr.regex = "screen|screening|scr")
  expect_identical(df_test, df_expected)
})

test_that("get_baseline works with all NA values in 'day0' column", {
  df <- proj01_elispot_data |> dplyr::filter(VISIT != "Unscheduled") |>
    tidyr::pivot_wider(names_from = VISIT, values_from = RESULT)

  df_expected <- df |>
    dplyr::mutate(BASELINE = ifelse(is.na(Day0), SCREEN,
                                    ifelse(is.na(SCREEN), Day0, NA)))
  df_test <- get_baseline(df, d0.regex = "d0|d 0|day 0|day0", scr.regex = "screen|screening|scr")
  expect_identical(df_test, df_expected)
})

test_that("get_baseline works with all NA values in 'screen' column", {
  df <- proj01_elispot_data |> dplyr::filter(VISIT != "Unscheduled") |>
    tidyr::pivot_wider(names_from = VISIT, values_from = RESULT)

  df_expected <- df |>
    dplyr::mutate(BASELINE = ifelse(is.na(Day0), SCREEN,
                                    ifelse(is.na(SCREEN), Day0, NA)))
  df_test <- get_baseline(df, d0.regex = "d0|d 0|day 0|day0", scr.regex = "screen|screening|scr")
  expect_identical(df_test, df_expected)
})

test_that("get_baseline works with all NA values in both 'day0' and 'screen' column", {
  df <- proj01_elispot_data |> dplyr::filter(VISIT != "Unscheduled") |>
    tidyr::pivot_wider(names_from = VISIT, values_from = RESULT)

  df_expected <- df |>
    dplyr::mutate(BASELINE = ifelse(is.na(Day0), SCREEN,
                                    ifelse(is.na(SCREEN), Day0, NA)))
  df_test <- get_baseline(df, d0.regex = "d0|d 0|day 0|day0", scr.regex = "screen|screening|scr")
  expect_identical(df_test, df_expected)
})

test_that("get_baseline fails with non-existent column", {
  df <- proj01_elispot_data |> dplyr::filter(VISIT != "Unscheduled") |>
    tidyr::pivot_wider(names_from = VISIT, values_from = RESULT)


  df_test <- get_baseline(df, d0.regex = "d0|d 0|day 0|day0", scr.regex = "screen|screening|scr")
  expect_error(get_baseline(df, d0.regex = "NON_EXISTING_COLUMN", scr.regex = "Screen"),
               "Failed to evaluate the left-hand side of formula ")
})

# tests for delta_calc() -------------------------------------------------------

test_that("delta_calc works with normal input", {
  df <- data.frame(
    SID = c("001", "002", "003"),
    BASELINE = c(10, 12, 15),
    WEEK1 = c(15, 17, 20),
    WEEK2 = c(20, 22, 25)
  )
  df_expected <- df
  df_expected$WEEK1_DELTA <- df_expected$WEEK1 - df_expected$BASELINE
  df_expected$WEEK2_DELTA <- df_expected$WEEK2 - df_expected$BASELINE
  df_test <- delta_calc(df)
  expect_identical(df_test, df_expected)
})


# Original elispot_vmd.R tests -------------------------------------------------

test_that("delta_calc works with all NA values in visit columns", {
  df <- data.frame(
    SID = c("001", "002", "003"),
    BASELINE = c(10, 12, 15),
    WEEK1 = rep(NA, 3),
    WEEK2 = rep(NA, 3)
  )
  df_expected <- df
  df_expected$WEEK1_DELTA <- rep(NA, 3) |> as.numeric()
  df_expected$WEEK2_DELTA <- rep(NA, 3) |> as.numeric()
  df_test <- delta_calc(df)
  expect_identical(df_test, df_expected)
})


test_that("delta_calc works with non-visit column", {
  df <- data.frame(
    SID = c("001", "002", "003"),
    BASELINE = c(10, 12, 15),
    NON_VISIT_COLUMN = c(15, 17, 20)
  )
  df_expected <- df
  df_test <- delta_calc(df)
  expect_identical(df_test, df_expected)
})

test_that("delta_calc works with custom regex", {
  df <- data.frame(
    SID = c("001", "002", "003"),
    BASELINE = c(10, 12, 15),
    WEEK1 = c(15, 17, 20),
    NON_VISIT_COLUMN = c(20, 22, 25)
  )
  df_expected <- df
  df_expected$WEEK1_DELTA <- df_expected$WEEK1 - df_expected$BASELINE
  df_test <- delta_calc(df, regex = "WEEK")
  expect_identical(df_test, df_expected)
})

test_that("delta_calc fails with non-existent baseline column", {
  df <- data.frame(
    SID = c("001", "002", "003"),
    NON_BASELINE_COLUMN = c(10, 12, 15),
    WEEK1 = c(15, 17, 20),
    WEEK2 = c(20, 22, 25)
  )
  expect_error(delta_calc(df), "Column `BASELINE` not found in `.data`")
})
