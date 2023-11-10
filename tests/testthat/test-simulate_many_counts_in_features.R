test_that("simulate_many_counts_in_features", {
  # Set up test parameters
  number_of_simulations <- 3
  number_of_random_positions <- 100
  chromosome_lengths <- data.frame(seqname = c("chr1", "chr2"), chr_length = c(500, 5000))
  overlap_window_sizes <- c(10, 20, 30)
  base_random_seed_value <- 123

  # Create test feature data 1
  test_feature_data_1 <- data.frame(
    seqname = c('chr1', 'chr2', 'chr2'),
    start = c(50, 100, 600),
    end = c(150, 201, 700),
    width = c(1, 1, 1),
    strand = c('*', '*', '*')
  )
  test_feature_data_1 <- GenomicRanges::makeGRangesFromDataFrame(test_feature_data_1)


  # Create a temporary file for the test feature data
  test_feature_file_1 <- base::tempfile(fileext = ".rds")
  saveRDS(test_feature_data_1, test_feature_file_1)

  # Create test feature data 2
  test_feature_data_2 <- data.frame(
    seqname = c('chr1', 'chr2', 'chr2'),
    start = c(50, 100, 3000),
    end = c(150, 201, 4000),
    width = c(1, 1, 1),
    strand = c('*', '*', '*')
  )
  test_feature_data_2 <- GenomicRanges::makeGRangesFromDataFrame(test_feature_data_2)

  # Create a temporary file for the test feature data
  test_feature_file_2 <- base::tempfile(fileext = ".rds")
  saveRDS(test_feature_data_2, test_feature_file_2)

  feature_files <- c(test_feature_file_1, test_feature_file_2)

  result <- simulate_many_counts_in_features(
    number_of_simulations,
    number_of_random_positions,
    chromosome_lengths,
    overlap_window_sizes,
    feature_files,
    base_random_seed_value
  )

  testthat::expect_true(nrow(result) == 18) # Number of combinations of number_of_simulations, length overlap window size, and number of features tested (3 * 3 * 2)

  result_feature_1 <- result %>%
    dplyr::filter(feature_track == test_feature_file_1) %>%
    dplyr::pull(count_in_feature) %>%
    base::sum()

  result_feature_2 <- result %>%
    dplyr::filter(feature_track == test_feature_file_2) %>%
    dplyr::pull(count_in_feature) %>%
    base::sum()

  testthat::expect_true(result_feature_2 > result_feature_1) # More integrations called for feature 2

  testthat::expect_true(max(result$sim_n) == number_of_simulations)

  testthat::expect_true("overlap_window_size" %in% colnames(result)) #, "Column 'overlap_window_size' should be present")
  testthat::expect_true("feature_track" %in% colnames(result)) #, "Column 'feature_track' should be present")
  testthat::expect_true("sim_n" %in% colnames(result)) #, "Column 'sim_n' should be present")
  testthat::expect_true("count_in_feature" %in% colnames(result)) #, "Column 'count_in_feature' should be present")
  testthat::expect_true("total_sites" %in% colnames(result)) #, "Column 'total_sites' should be present")
  testthat::expect_true("percentage_integrations_in_feature" %in% colnames(result)) #, "Column 'percentage_integrations_in_feature' should be present")


})
