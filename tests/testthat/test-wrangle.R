# optimized tests --------------------------------------------------------------

# get_bfxpkg_data_names() tests ------------------------------------------------
test_that("get_bfxpkg_data_names returns the correct dataset names", {
  expect_equal(get_bfxpkg_data_names(), c(
    "visit_recoding", "flow01_count_data", "flow_markerlist", "markers_flow", "flow_qcmeet_input",
    "FLOW_intermediate_data", "elispot01_data", "elispot02_data"
  ))
})

test_that("get_dat correctly returns dataset", {
  # This test depends on the datasets actually present in the package.
  expect_s3_class(get_dat(data.name = "visit_recoding"), "data.frame")

  # Raise error for incorrect dataset name
  expect_error(get_dat(data.name = "incorrect_dataset_name"))
})



# get_date() tests -------------------------------------------------------------
test_that("get_date returns a string in the expected format", {
  expect_true(str_detect(get_date(), "^[0-9]{2}[A-Za-z]{3}[0-9]{2}$"))
})

test_that("get_date correctly returns date", {
  expect_equal(get_date(), strftime(Sys.Date(), format = "%d%b%y"))
})



# get_initials() tests ---------------------------------------------------------
test_that("get_initials returns correct initials for known users", {

  current_user <- tolower(Sys.info()["user"])

  testthat::skip_if(stringr::str_detect(string = current_user,
                                        pattern = "runner.*"
  ))

  user_initials <- switch(current_user,
                          "artemio.sison" = "MS3",
                          "user01" = "u1",
                          "user02" = "u2",
                          "user03" = "u3",
                          "user04" = "u4",
                          "runner" = "GIT",
                          "runneradmin" = "GIT")

  # Assuming the function is called within the system context of the known user
  expect_equal(get_initials(), user_initials)
})

# test_that("get_initials correctly returns user initials", {
#   # This test depends on the actual users on your system
#   # Replace "artemio.sison" with an actual username on your system
#   # Sys.setenv(USER = "artemio.sison")
#
#
#   initials <- get_initials()
#
#   testthat::skip_if(stringr::str_detect(string = initials,
#                                         pattern = "git.*"
#   ))
#   #
#   expect_equal(initials, "MS3")
#
#   # Returns NULL for unknown user - needs to be added to fn
#   # Sys.setenv(USER = "unknown_user")
#   # expect_equal(get_initials(), NULL)
# })



# get_user() tests -------------------------------------------------------------
test_that("get_user returns the current username in lowercase", {
  expect_equal(get_user(), tolower(Sys.info()['user']))
})


# original tests ---------------------------------------------------------------

# get_date() -------------------------------------------------------------------
test_that("get_date() returns a character string", {
  output_obj <- get_date()
  expected_obj <- strftime(Sys.Date(), format = "%d%b%y")

  expect_equal(object = output_obj,
               expected = expected_obj)
})

test_that("get_date() returns a character", {
  expect_type(object = get_date(),
              type = "character")
})


# add.drop_sid_hyphen tests() --------------------------------------------------
test_that("add.drop_sid_hyphen adds hyphen correctly", {
  expect_equal(add.drop_sid_hyphen("12345678", "add"), "1234-5678")
  expect_equal(add.drop_sid_hyphen("98761234", "add"), "9876-1234")
})

test_that("add.drop_sid_hyphen drops hyphen correctly", {
  expect_equal(add.drop_sid_hyphen("1234-5678", "drop"), "12345678")
  expect_equal(add.drop_sid_hyphen("9876-1234", "drop"), "98761234")
})

test_that("add.drop_sid_hyphen handles edge cases", {
  expect_error(add.drop_sid_hyphen("1234-567", "add"))
  expect_error(add.drop_sid_hyphen("1234-56789", "add"))
  expect_error(add.drop_sid_hyphen("1234567", "drop"))
  expect_error(add.drop_sid_hyphen("123456789", "drop"))
})

test_that("add.drop_sid_hyphen validates add.drop argument", {
  expect_error(add.drop_sid_hyphen("12345678", "random_string"))
  expect_error(add.drop_sid_hyphen("1234-5678", "123"))
})

test_that("add.drop_sid_hyphen correctly adds hyphen", {
  expect_equal(add.drop_sid_hyphen("12345678", "add"), "1234-5678")
})
test_that("add.drop_sid_hyphen correctly drops hyphen", {
  expect_equal(add.drop_sid_hyphen("1234-5678", "drop"), "12345678")
})
test_that("add.drop_sid_hyphen throws error for invalid arguments", {
  expect_error(add.drop_sid_hyphen("1234-5678", "invalid"))
})



# original code for accessing columns() ----------------------------------------
# test_that("elispot_DM_names returns the correct column names", {
#   expect_equal(elispot_DM_names(), c("SID", "STIM", "VISIT", "RESULT"))
# })
#
# # elisa_DM_names
# test_that("elisa_DM_names returns the correct column names", {
#   expect_equal(elisa_DM_names(), c("SID", "STIM", "VISIT", "RESULT"))
# })
#
# # algl_DM_names
# test_that("algl_DM_names returns the correct column names", {
#   expect_equal(algl_DM_names(), c("SID", "STIM", "VISIT", "PARAM", "RESULT"))
# })
