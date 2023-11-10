# Mock data for testing
simulation_results <- data.frame(
  feature_track = c("feature1", "feature2", "feature1", "feature2"),
  overlap_window_size = c(10, 10, 20, 20),
  percentage_integrations_in_feature = c(5, 10, 15, 20)
)

expand_data <- function(data, n, variability = 0.1) {
  expanded_data <- data[rep(seq_len(nrow(data)), each = n), ]
  expanded_data$percentage_integrations_in_feature <- expanded_data$percentage_integrations_in_feature * (1 + rnorm(n * nrow(data), mean = 0, sd = variability))
  return(expanded_data)
}

# Specify the desired number of rows per combination and variability
rows_per_combination <- 100
variability <- 0.1  # Adjust the variability as needed

# Expand the data with variability
expanded_simulation_results <- expand_data(simulation_results, rows_per_combination, variability)


experimental_probabilities <- data.frame(
  feature_track = c("feature1", "feature2", "feature1", "feature2", 'feature1' ,'feature2'),
  overlap_window_size = c(10, 10, 20, 20, 10, 20),
  heatmap_group = c("A", "B", "A", "B", "C", "C"),
  percentage_integrations_in_feature = c(5, 10, 15, 20, 3, 25)
)

testthat::test_that("integration_percentage_in_feature_p_value works", {

  # Call the function
  result <- integration_percentage_in_feature_p_value(expanded_simulation_results, experimental_probabilities)

  testthat::expect_true("data.frame" %in% class(result)) # result should be dataframe

  testthat::expect_true(all(c("feature_track", "overlap_window_size", "heatmap_group", "percentage_integrations_in_feature",
                    "probability", "p_value", "sd_from_mean") %in% colnames(result))) # Check if the expected columns are present

  testthat::expect_true(
    result %>%
      dplyr::filter(heatmap_group == 'C', overlap_window_size == 10) %>%
      dplyr::pull(p_value) == 0
  ) # Test ecdf p value calculation worked correctly

  testthat::expect_true(
    result %>%
      dplyr::filter(heatmap_group == 'C', overlap_window_size == 10) %>%
      dplyr::pull(probability) == 0
  ) # Test ecdf probability calculation worked correctly and is close to 0

  testthat::expect_true(
    result %>%
      dplyr::filter(heatmap_group == 'C', overlap_window_size == 20) %>%
      dplyr::pull(p_value) < 0.5
  ) # Test ecdf p value calculation worked correctly

  testthat::expect_true(
    result %>%
      dplyr::filter(heatmap_group == 'C', overlap_window_size == 20) %>%
      dplyr::pull(probability) > 0.95
  ) # Test ecdf probability calculation worked correctly and is close to 1

})
