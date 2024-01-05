## VISITs given in a function call ---------------------------------------------

test_that("VISITs are type 'character' ", {
  expect_type(visit_test(), type = "character")
})

test_that("VISITs are not null with arg 'proj01'", {
  expect_length(
    object = visit_test(v.study = "proj01"),
    n = 9
  )
})

test_that("VISITs are not null with arg 'proj02'", {
  expect_length(
    object = visit_test(v.study = "proj02"),
    n = 9
  )
})

test_that("VISITs are not null with arg 'proj03'", {
  expect_length(
    object = visit_test(v.study = "proj03"),
    n = 9
  )
})

test_that("VISITs are not null with arg 'flow01'", {
  expect_length(
    object = visit_test(v.study = "flow01"),
    n = 8
  )
})

test_that("VISITs have length 207 arg 'any'", {
  expect_length(
    object = visit_test(v.study = "any"),
    n = 207
  )
})

test_that("VISITs have length 207 with no arg supplied", {
  expect_length(
    object = visit_test(),
    n = 207
  )
})

test_that("VISITs throws error when incorrect arg supplied ", {
  expect_error(
    object = visit_test(v.study = "wrong")
  )
})


  ## VISITs are recoded ----------------------------------------------------------

test_that("Visits `in a string` are recoded (full name)", {
  t1 <- "Week 1"
  expect_equal(
    object = recode_visits(t1, output.format = "full"),
    expected = "Week1"
    )
})

test_that("Visits `in a string` are recoded (abbreviated)", {
  t1 <- "Week 1"
  expect_equal(
    object = recode_visits(t1, output.format = "abbr"),
    expected = "WK1"
  )
})

test_that("No argument supplied throws an error", {
  t1 <- "Week 1"
  expect_error(
    object = recode_visits(v.string = t1, output.format = "wrong"),
    regexp = ".*Error.*")
})
