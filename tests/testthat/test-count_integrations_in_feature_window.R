
test_that("count_integrations_in_feature_window correctly counts integrations", {
  # Create an experimental test data frame
  test_df <- data.frame(
    seqname = c('chr1', 'chr2', 'chr2'),
    start = c(100, 200, 300),
    end = c(100, 200, 300),
    width = c(1, 1, 1),
    strand = c('*', '*', '*'),
    type = c('insertion', 'insertion', 'insertion'),
    heatmap_group = c('testset_subject1_sample1', 'testset_subject1_sample1', 'testset_subject1_sample1')
  )

  # Create a simulated test dataframe
  test_simulated_df <- data.frame(
    seqname = c('chr1', 'chr2', 'chr2'),
    random_positions = c(100, 200, 300)
  )

  # Create test feature data
  test_feature_data <- data.frame(
    seqname = c('chr1', 'chr2', 'chr2'),
    start = c(50, 100, 600),
    end = c(150, 201, 700),
    width = c(1, 1, 1),
    strand = c('*', '*', '*')
  )
  test_feature_data <- GenomicRanges::makeGRangesFromDataFrame(test_feature_data)


  # Create a temporary file for the test feature data
  test_feature_file <- base::tempfile(fileext = ".rds")
  saveRDS(test_feature_data, test_feature_file)

  # Define the overlap window size
  test_overlap_window_size <- 10

  # Call the function
  result <- count_integrations_in_feature_window(test_df, test_feature_file, test_overlap_window_size)

  # Check the results
  testthat::expect_equal(length(result), 3) #"Expected result to have three elements")

  # Check the count of integrations
  testthat::expect_true(result[[1]] >= 0) #"Integration count should be a non-negative number")

  # Check the total integration sites
  testthat::expect_equal(result[[2]], nrow(test_df)) # "Total integration sites should match the number of rows in the test data frame")

  # Check the percentage of integration sites
  testthat::expect_true(0 <= result[[3]]) # "Percentage of integration sites should be between 0 and 100")
  testthat::expect_true(100 >= result[[3]]) # "Percentage of integration sites should be between 0 and 100")

  # Test sample_genome() input
  result_random <- count_integrations_in_feature_window(test_simulated_df, test_feature_file, test_overlap_window_size)
  testthat::expect_equal(result, result_random) # Simulated and experimental should give the same result

  # Expanding the window range should result in 3 of 3 sites being found
  test_overlap_window_size_expanded <- 1000
  result <- count_integrations_in_feature_window(test_df, test_feature_file, test_overlap_window_size_expanded)
  testthat::expect_equal(result, c(3, 3, 100))

  # Clean up the temporary file
  base::unlink(test_feature_file)
})
