
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
  feature_track = c("feature1", "feature1", "feature1", "feature2", 'feature2' ,'feature2'),
  overlap_window_size = c(10, 10, 10, 20, 20, 20),
  heatmap_group = c("A", "B", "C", "A", "B", "C"),
  percentage_integrations_in_feature = c(5, 10, 15, 20, 3, 25)
)


testthat::test_that("plot_integration_percentages generates ggplot objects", {

  result_plots <- plot_integration_percentages(expanded_simulation_results, experimental_probabilities, 0.1, TRUE)

  testthat::expect_true(class(result_plots) == 'list')

  testthat::expect_true(length(result_plots) == 2)

  testthat::expect_true(class(result_plots[[1]])[2] == 'ggplot')

})
