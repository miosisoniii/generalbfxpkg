# optimized tests --------------------------------------------------------------

# Test for import_flow_freq
test_that("Test import_flow_freq", {
  sp_path <- load_sp(type = "dev")

  # Mock for import_flow_freq
  mock_wsp_path <- "mock_path.wsp"
  expect_error(import_flow_freq(mock_wsp_path), "File 'mock_path.wsp' does not exist.")
})

# Test for convert_freq_pct
test_that("Test convert_freq_pct", {
  expect_equal(convert_freq_pct(c(0.2, 0.4, 0.3, 0.1)), c(20, 40, 30, 10))
  expect_equal(convert_freq_pct(c(0.2, 0.4, 0.3, 0.1), TRUE), c(20, 40, 30, 10))
})

# Test for format_long_freq
test_that("Test format_long_freq", {
  # # Define some mock data
  # mock_data <- data.frame(
  #   name = c("Sample1", "Sample2"),
  #   PARAM = c("CD4", "CD8"),
  #   Frequency = c(55.0, 30.0)
  # )

  mock_data <- flow01_count_data |>
    tidyr::unite(col = "name", c("SID", "VISIT", "STIM"), sep = "_")
  df <- format_long_freq(mock_data, FALSE)
  expect_equal(nrow(df), 336)
  expect_equal(ncol(df), 3)
  expect_equal(colnames(df), c("name", "PARAM", "Frequency"))
})

# Test for flow_preqc_wrap
test_that("Test flow_preqc_wrap", {
  # Mock for flow_preqc_wrap
  mock_path_to_analyst_dir <- "mock_path"

  expect_error(flow_preqc_wrap(mock_path_to_analyst_dir),
               "Path does not exist")
})

# Test for flow_qc_compare
test_that("Test flow_qc_compare", {
  # # Define some mock data
  mock_data <- data.frame(
    name = c("Sample1", "Sample2"),
    PARAM = c("CD4", "CD8"),
    Frequency = c(55.0, 30.0)
  )
  df1 <- mock_data
  df2 <- mock_data %>% mutate(Frequency = c(54.5, 29.5))

  cv_df <- flow_qc_compare(df1, df2)
  expect_equal(nrow(cv_df), 2)
  expect_equal(ncol(cv_df), 6)
  expect_equal(colnames(cv_df), c("name", "PARAM", "Frequency_a1", "Frequency_a2", "cv", "qc_pass"))
})



# Test convert_freq_pct function
test_that("Test convert_freq_pct", {

  # Test 1: Check if the function correctly converts a numeric vector of frequencies to percentages.
  expect_equal(convert_freq_pct(c(0.2, 0.4, 0.3, 0.1)), c(20, 40, 30, 10))

  # Test 2: Check if the function correctly rounds the percentages when the round argument is TRUE.
  expect_equal(convert_freq_pct(c(0.12345, 0.67890), round = TRUE), c(12.345, 67.890))

  # Test 3: Check if the function works with a vector of length 1.
  expect_equal(convert_freq_pct(c(0.5)), 50)
})


# Test flow_qc_compare function
test_that("Test flow_qc_compare", {

  # Test 1: Check if the function correctly compares flow cytometry data from two analysts.
  analyst1_data <- data.frame(name = c("Sample1", "Sample2"),
                              PARAM = c("CD4", "CD8"),
                              Frequency = c(55.0, 30.0))
  analyst2_data <- data.frame(name = c("Sample1", "Sample2"),
                              PARAM = c("CD4", "CD8"),
                              Frequency = c(54.5, 29.5))
  comparison_result <- flow_qc_compare(analyst1_data, analyst2_data)
  expect_s3_class(comparison_result, "tbl_df")

  # Test 2: Check if the function correctly calculates the coefficient of variation (CV).
  expect_equal(round(comparison_result$cv[1], 3), 0.646)

  # Test 3: Check if the function correctly determines if the data pass or fail the quality control criteria.
  expect_equal(comparison_result$qc_pass[1], "PASS")

  # Test 4: Check if data has unequal amount of rows - expect error
  a1_data <- analyst1_data[1,]
  expect_error(flow_qc_compare(analyst1.df = a1_data,
                               analyst2.df = analyst2_data),
               "Differing number of rows")
})





# original tests ---------------------------------------------------------------
test_that("file path to .wsp does not exist", {
  expect_error(
    object = import_flow_freq(path.to.wsp = "")
  )
})



## Testing for convert_freq_pct() ----------------------------------------------


test_that("convert_freq_pct works correctly without rounding", {
  x <- 0.25
  expected_result <- 25

  result <- convert_freq_pct(x, round = FALSE)
  expect_equal(result, expected_result,
               info = "convert_freq_pct without rounding produces incorrect result")
})

test_that("convert_freq_pct works correctly with rounding", {
  x <- 0.25678
  expected_result <- 25.678

  result <- convert_freq_pct(x, round = TRUE)
  expect_equal(result, expected_result,
               info = "convert_freq_pct with rounding produces incorrect result")
})


## Testing for the QC formatting functions here --------------------------------

test_that("format_long_freq() has correct metadata cols from split metadata (flow01)", {
  data(flow01_count_data)
  expect_true(
    all(c("SID", "VISIT", "STIM") %in% names(flow01_count_data))
    )
})

test_that("format_long_freq() has correct metadata cols from split metadata (proj03)", {
  ## this data loaded contains the full filename, not separated into 'SID' 'VISIT' 'STIM'
  ## load from '/data' qc_meeting input (contains 2 analyst df's and qcmeeting input df)
  data(flow_qcmeet_input)
  ## select EG data from the list obj
  proj03_analyst1_dat <- flow_qcmeet_input[["proj03_analyst1"]] %>%
    head() %>%
    tidyr::separate(`name`, into = c("SID", "VISIT", "STIM"), sep = "_") %>%
    suppressWarnings()

  expect_true(
    all(c("SID", "VISIT", "STIM") %in% names(proj03_analyst1_dat))
  )
})

test_that("format_long_freq() has output metadata col of 'name' when meta.split = TRUE ", {
  data(flow01_count_data)
  format_long_df <- format_long_freq(flow01_count_data, meta.split = TRUE)
  expect_true(
    all(c("SID", "VISIT", "STIM", "PARAM", "Frequency") %in% names(format_long_df))
  )
})




## flow_qc_compare() to compare analyst df's -----------------------------------
