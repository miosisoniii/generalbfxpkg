## Anonymization of SID (dataframe) --------------------------------------------

test_that("input df has an SID column", {
  data(flow01_count_data)
  expect_equal(names(flow01_count_data)[1], expected = "SID")
})

test_that("Output df col SID is a character vector", {
  data("flow01_count_data")
  expect_equal(
    object = names(anonymize_sid(flow01_count_data))[1],
    expected = "SID")
})

test_that("Error if no column is named SID in input df", {
  data(flow01_count_data)
  error_df <- flow01_count_data |>
    dplyr::rename("S" = "SID")
  expect_error(
    object = anonymize_sid(error_df) )
})

## Anonymization of metadata (SID, VISIT, STIM, PARAM, miRNA) ------------------
test_that("subjects were anonymized", {
  data(flow01_count_data)
  orig_sid <- flow01_count_data$SID[1]
  new_sid <- flow01_count_data |>
    mutate(SID = anonymize_meta(.data$SID, type = "SID")) |>
    pull(.data$SID) |>
    unique()

  expect_false(orig_sid == new_sid)
})

test_that("subject column is a character vector", {
  data(flow01_count_data)
  expect_type(anonymize_meta(flow01_count_data$SID, type = "SID"), type = "character")
})

test_that("subjects after anonymization are not the same as original", {
  data(flow01_count_data)
  expect_failure(
    expect_mapequal(object = anonymize_meta(flow01_count_data$SID, type = "SID"),
                    expected = flow01_count_data$SID)
  )

})

test_that("SID column is a character", {
  data(flow01_count_data)
  expect_type(
    object = anonymize_meta(flow01_count_data$SID, type = "SID"),
    type = "character")

})

test_that("VISIT column is a character", {
  data(flow01_count_data)
  expect_type(
    object = anonymize_meta(flow01_count_data$VISIT, type = "VISIT"),
    type = "character")

})

test_that("STIM column is a character", {
  data(flow01_count_data)
  expect_type(
    object = anonymize_meta(flow01_count_data$STIM, type = "STIM"),
    type = "character")

})

test_that("MIRNA column is a character", {
  data(flow01_count_data)
  expect_type(
    object = anonymize_meta(flow01_count_data$VISIT, type = "MIRNA"),
    type = "character")

})

test_that("PARAM column is a character", {
  ex_df <- data.frame(
    SID = paste0("SID", sample(10002000:3000000, 6)),
    VISIT = paste0("WEEK", sample(1:10, 6)),
    STIM = paste0("HPV", sample(1:3, 3)),
    PARAM = paste0("CD", sample(1:25, 3)),
    MIRNA = paste0("hsa.mir", sample(1:25, 3)))

  expect_type(
    object = anonymize_meta(ex_df$PARAM, type = "PARAM"),
    type = "character")

})
