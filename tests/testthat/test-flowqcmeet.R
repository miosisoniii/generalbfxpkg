# optimized tests --------------------------------------------------------------

# index_cv_fail() --------------------------------------------------------------
test_that("index_cv_fail handles empty data frame", {
  data <- data.frame()
  actual_result <- index_cv_fail(data)

  expected_result <- which(data > 20, arr.ind = TRUE) # Expecting an empty matrix

  expect_equal(dim(actual_result), dim(expected_result))
  expect_equal(dimnames(actual_result), dimnames(expected_result))
  expect_equal(actual_result, expected_result)
})


test_that("index_cv_fail handles all zeros correctly", {
  data <- data.frame(A = c(0, 0), B = c(0, 0))
  actual_result <- index_cv_fail(data)

  expected_result <- integer(0) # Expecting no indices (empty vector)

  expect_equal(actual_result, expected_result)
})

test_that("index_cv_fail handles empty data frame", {
  data <- data.frame()
  actual_result <- index_cv_fail(data) |> as.list()

  expected_result <- integer(0) |> as.list()# Expecting no indices (empty vector)

  expect_equal(actual_result, expected_result)
})


# import_cv_xl() ---------------------------------------------------------------

# test_that("import_cv_xl creates xlsx file", {
#   data <- data.frame(A = c(19, 21), B = c(22, 18))
#   result <- import_cv_xl(data)
#
#   # Ensure the temporary file is deleted when the test finishes
#   # on.exit({
#   #   if (file.exists(result$output.path)) {
#   #     file.remove(result$output.path)
#   #   }
#   # }, add = TRUE)
#
#   # Check if the xlsx file was created
#   expect_true(file.exists(result$output.path))
# })

# test_that("import_cv_xl writes the correct data to the file", {
#   data <- data.frame(A = c(19, 21), B = c(22, 18))
#   result <- import_cv_xl(data)
#
#   # Read the data from the "QC" sheet
#   actual_data <- openxlsx::read.xlsx(xlsxFile = result$output.path)
#
#   # Check if the correct data was written to the file
#   expect_equal(actual_data, data)
# })

# write_cv_xl ------------------------------------------------------------------

# test_that("write_cv_xl creates an xlsx file", {
#   data <- flow_qcmeet_input[[3]]
#   temp_path <- tempdir()
#   write_cv_xl(data, output.dir = temp_path)
#   expect_true(file.exists(temp_path))
# })


# original tests ---------------------------------------------------------------
## testing input
test_that("input to index_cv_fail is a dataframe", {
  cv_idf <- flow_qcmeet_input[[3]]
  expect_s3_class(object = cv_idf,
                  class = "data.frame")
})


test_that("index_cv_fail input has correct colnames", {
  expected_colnames <- c("name", "PARAM", "Frequency_a1", "Frequency_a2", "cv", "qc_pass")
  cv_idf <- flow_qcmeet_input[[3]]

  expect_named(object = cv_idf,
               expected = expected_colnames,
               ignore.order = TRUE,
               ignore.case = TRUE)
})


test_that("index_cv_fail returns an array", {
  cv_idf <- flow_qcmeet_input[[3]]
  expected_class <- index_cv_fail(cv_idf)
  expect_type(object = expected_class, type = "integer")
})



