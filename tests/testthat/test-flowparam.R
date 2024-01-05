# Test get_flow_params function
test_that("Test get_flow_params", {
  # Test 1: Check if the function correctly extracts unique parameters from the Population column.
  # Construct test data
  test_data <- tibble::tibble(
    SID = rep("SID1", 5),
    VISIT = rep("VISIT1", 5),
    STIM = rep("STIM1", 5),
    Population = c("CD4/CD38", "CD8/CD38", "CD4/CD69", "CD8/CD69", "CD4/CD38"),
    Parent = rep("Parent1", 5),
    Frequency = rep(0.5, 5),
    ParentFrequency = rep(0.6, 5)
  )
  expect_equal(get_flow_params(test_data), c("CD38", "CD69"))

  # Test 2: Check if the function correctly handles an empty data frame.
  empty_data <- tibble::tibble()
  expect_error(get_flow_params(empty_data))

  # Test 3: Check if the function returns only unique parameters.
  duplicated_data <- test_data
  expect_equal(get_flow_params(duplicated_data), c("CD38", "CD69"))
})

# Test gen_flow_combos function
test_that("Test gen_flow_combos", {
  # Test 1: Check if the function correctly generates all combinations of a list of parameters.
  params <- c("CD38", "CD69", "Gnly")
  test_output <- gen_flow_combos(params)
  expect_equal(nrow(test_output), 2^length(params) - 1 + length(params)) # 2^n - 1 combinations plus n single parameters

  # Test 2: Check if the function correctly generates strings and string_regex columns.
  expect_setequal(colnames(test_output), c("strings", "strings_regex"))

  # Test 3: Check if the function correctly handles an empty parameter list.
  empty_params <- character(0)
  expect_error(nrow(gen_flow_combos(empty_params)))
})
