test_that("load_sp returns correct paths for dev and prod", {
  username <- tolower(Sys.info()['user'])

  testthat::skip_if(stringr::str_detect(string = username,
                                        pattern = "runner.*"
                                        ))

  expected_dev_path <- paste0("C:/Users/", username,
                              "/path/to/dev/")
  expected_prod_path <- paste0("C:/Users/", username,
                               "/path/to/prod")

  expect_equal(load_sp("dev"), expected_dev_path)
  expect_equal(load_sp("prod"), expected_prod_path)
})

test_that("load_sp is case-insensitive", {
  username <- tolower(Sys.info()['user'])

  testthat::skip_if(stringr::str_detect(string = username,
                                        pattern = "runner.*"
  ))

  expected_dev_path <- paste0("C:/Users/", username,
                              "/path/to/dev")
  expected_prod_path <- paste0("C:/Users/", username,
                               "/path/to/prod")

  expect_equal(load_sp("Dev"), expected_dev_path)
  expect_equal(load_sp("Prod"), expected_prod_path)
})

test_that("load_sp accepts full type names (Development and Production)", {
  username <- tolower(Sys.info()['user'])

  testthat::skip_if(stringr::str_detect(string = username,
                                        pattern = "runner.*"
  ))

  expected_dev_path <- paste0("C:/Users/", username,
                              "/path/to/dev")
  expected_prod_path <- paste0("C:/Users/", username,
                               "/path/to/prod")

  expect_equal(load_sp("Development"), expected_dev_path)
  expect_equal(load_sp("Production"), expected_prod_path)
})

test_that("load_sp defaults to 'dev' if no argument is provided", {
  expect_warning(load_sp(), regexp = ".*defaulting.*")
})

test_that("load_sp defaults to 'dev' if no argument is provided", {
  username <- tolower(Sys.info()['user'])

  testthat::skip_if(stringr::str_detect(string = username,
                                        pattern = "runner.*"
  ))

  expected_dev_path <- paste0("C:/Users/", username,
                              "/path/to/dev")

  empty_arg_path <- suppressWarnings(load_sp())
  expect_equal(empty_arg_path, expected_dev_path)
})

test_that("load_sp returns an error if an invalid type is provided", {
  expect_error(load_sp("invalid_type"), regexp = ".*should be.*")
})
