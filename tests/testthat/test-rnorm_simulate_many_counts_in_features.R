test_that("rnorm_simulate_many_counts_in_features truncated produces expected results", {
  # Set up test parameters
  test_simulation_results <- data.frame(
    overlap_window_size = rep(0, 11),
    feature_track = rep("feature_1", 11),
    sim_n = seq(1,11),
    count_in_feature = c(15, 14, 16, 12, 14, 10, 19, 23, 19, 14, 77),
    total_sites = rep(1000, 11)#,
  )

  test_simulation_results <- rbind(
    test_simulation_results,
    data.frame(
      overlap_window_size = rep(1000, 11),
      feature_track = rep("feature_1", 11),
      sim_n = seq(1,11),
      count_in_feature = c(150, 140, 160, 120, 140, 100, 190, 230, 190, 140, 770),
      total_sites = rep(1000, 11)#,
    ))

  test_simulation_results <- rbind(
    test_simulation_results,
    data.frame(
      overlap_window_size = rep(0, 11),
      feature_track = rep("feature_1", 11),
      sim_n = seq(1,11),
      count_in_feature = c(1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 7),
      total_sites = rep(1000, 11)#,
    ))

  test_simulation_results <- rbind(
    test_simulation_results,
    data.frame(
      overlap_window_size = rep(1000, 11),
      feature_track = rep("feature_1", 11),
      sim_n = seq(1,11),
      count_in_feature = c(15, 14, 16, 12, 14, 10, 19, 23, 19, 14, 77),
      total_sites = rep(1000, 11)#,
      # percentage_integrations_in_feature = c(1.5, 1.4, 1.6, 1.2, 1.4, 1.0, 1.9, 2.3, 1.9, 1.4, 7.7)
    ))

  test_simulation_results <- test_simulation_results %>%
    dplyr::mutate(
      percentage_integrations_in_feature = 100 * (count_in_feature/total_sites)
    )

  rnorm_value <- 100

  # Call the function
  result <- rnorm_simulate_many_counts_in_features(test_simulation_results, rnorm_value, truncated_distribution = TRUE, lower_bound = 0, upper_bound = 100)

  # Check the structure of the result
  testthat::expect_true(class(result) == "data.frame") # "Result should be a data frame")
  testthat::expect_equal(ncol(result), 3) #, "Result should have 3 columns")

  # Check if columns are present
  testthat::expect_true("percentage_integrations_in_feature" %in% colnames(result)) #, "Column 'percentage_integrations_in_feature' should be present")
  testthat::expect_true("overlap_window_size" %in% colnames(result)) #, "Column 'overlap_window_size' should be present")
  testthat::expect_true("feature_track" %in% colnames(result)) #, "Column 'feature_track' should be present")

  # Check the number of rows in the result
  testthat::expect_equal(nrow(result), rnorm_value * nrow(test_simulation_results)/11/2) #, "Unexpected number of rows in the result")

  # Check if simulated percentages are within expected ranges
  testthat::expect_true(all(result$percentage_integrations_in_feature >= 0 & result$percentage_integrations_in_feature <= 100)) #,"Simulated percentages should be within the range [0, 100]")
})

test_that("rnorm_simulate_many_counts_in_features rnorm produces expected results", {
  # Set up test parameters

  test_simulation_results <- data.frame(
    overlap_window_size = rep(0, 11),
    feature_track = rep("feature_1", 11),
    sim_n = seq(1,11),
    count_in_feature = c(15, 14, 16, 12, 14, 10, 19, 23, 19, 14, 77),
    total_sites = rep(1000, 11)#,
  )

  test_simulation_results <- rbind(
    test_simulation_results,
    data.frame(
      overlap_window_size = rep(1000, 11),
      feature_track = rep("feature_1", 11),
      sim_n = seq(1,11),
      count_in_feature = c(150, 140, 160, 120, 140, 100, 190, 230, 190, 140, 770),
      total_sites = rep(1000, 11)#,
    ))

  test_simulation_results <- rbind(
    test_simulation_results,
    data.frame(
      overlap_window_size = rep(0, 11),
      feature_track = rep("feature_1", 11),
      sim_n = seq(1,11),
      count_in_feature = c(1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 7),
      total_sites = rep(1000, 11)#,
    ))

  test_simulation_results <- rbind(
    test_simulation_results,
    data.frame(
      overlap_window_size = rep(1000, 11),
      feature_track = rep("feature_1", 11),
      sim_n = seq(1,11),
      count_in_feature = c(15, 14, 16, 12, 14, 10, 19, 23, 19, 14, 77),
      total_sites = rep(1000, 11)#,
      # percentage_integrations_in_feature = c(1.5, 1.4, 1.6, 1.2, 1.4, 1.0, 1.9, 2.3, 1.9, 1.4, 7.7)
    ))

  test_simulation_results <- test_simulation_results %>%
    dplyr::mutate(
      percentage_integrations_in_feature = 100 * (count_in_feature/total_sites)
    )

  rnorm_value <- 100

  # Call the function
  result <- rnorm_simulate_many_counts_in_features(test_simulation_results, rnorm_value, truncated_distribution = FALSE, lower_bound = 0, upper_bound = 100)

  # Check the structure of the result
  testthat::expect_true(class(result) == "data.frame") # "Result should be a data frame")
  testthat::expect_equal(ncol(result), 3) #, "Result should have 3 columns")

  # Check if columns are present
  testthat::expect_true("percentage_integrations_in_feature" %in% colnames(result)) #, "Column 'percentage_integrations_in_feature' should be present")
  testthat::expect_true("overlap_window_size" %in% colnames(result)) #, "Column 'overlap_window_size' should be present")
  testthat::expect_true("feature_track" %in% colnames(result)) #, "Column 'feature_track' should be present")

  # Check the number of rows in the result
  testthat::expect_equal(nrow(result), rnorm_value * nrow(test_simulation_results)/11/2) #, "Unexpected number of rows in the result")

  # Check if simulated percentages are within expected ranges. This is not a truncated distribution and the mean of some values is 1 so the sd will make rnorm sample below 0.
  testthat::expect_false(all(result$percentage_integrations_in_feature >= 0 & result$percentage_integrations_in_feature <= 100)) #,"Simulated percentages should be within the range [0, 100]")
})

