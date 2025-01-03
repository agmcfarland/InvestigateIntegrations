test_that("find_groups_of_integration_positions works", {

  test_positions_to_check <- c(
    100, 101, 200,
    2000, 2500, 3000, 3001, 3001, 3001,
    5000,
    9000,
    50
  )

  test_overlap_search_range <- 1000

  test_output <- find_groups_of_integration_positions(test_positions_to_check, test_overlap_search_range, min_group_size = 1)

  testthat::expect_equal(length(test_output), 4)

  testthat::expect_equal(length(test_output[[2]]), 6)


  test_positions_to_check <- c(1, 2, 3)

  test_output <- find_groups_of_integration_positions(test_positions_to_check, test_overlap_search_range, min_group_size = 1)

  testthat::expect_equal(length(test_output), 1)

  testthat::expect_equal(length(test_output[[1]]), 3)


  test_positions_to_check <- c(1, 200000, 3000000)

  test_output <- find_groups_of_integration_positions(test_positions_to_check, test_overlap_search_range, min_group_size = 2)

  testthat::expect_equal(length(test_output), 0)


  test_positions_to_check <- c(1, 1, 200000, 3000000)

  test_output <- find_groups_of_integration_positions(test_positions_to_check, test_overlap_search_range, min_group_size = 2)

  testthat::expect_equal(length(test_output), 1)

  testthat::expect_equal(length(test_output[[1]]), 2)



})
