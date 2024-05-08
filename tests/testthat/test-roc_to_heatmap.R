testthat::test_that("test_for_overlaps works", {

  example_aavenger_table <- read.csv(testthat::test_path('testdata', 'example_aavenger_table.csv'))

  example_hg38_chromosome_lengths <- read.csv(testthat::test_path('testdata', 'example_hg38_chromosome_lengths.csv'))

  formatted_example_aavenger_table <- format_aavenger_sites(example_aavenger_table)

  match_row_number_modifier_ <- 3

  random_match_table <- aavenger_sites_random_match(
    aavenger_sites = formatted_example_aavenger_table,
    chromosome_lengths = example_hg38_chromosome_lengths,
    random_seed_value = 10,
    match_row_number_modifier = match_row_number_modifier_)

  combined_df <- rbind(formatted_example_aavenger_table, random_match_table)

  list_of_feature_files_ <- list.files(testthat::test_path('testdata'), pattern = "refGene.rds|H3K79*", full.names = TRUE)

  overlap_ranges_to_test_ <- c(0, 100, 1000, 10000)

  # Test RData format works
  overlap_test_result <- test_for_overlaps(
    matched_aavenger_sites_df = combined_df,
    list_of_feature_files = list_of_feature_files_,
    overlap_ranges_to_test = overlap_ranges_to_test_
  )

  hot_roc_result <- hotroc_compare_insertion_to_match(
    matched_overlap_df = overlap_test_result
  )

  p1 <- roc_to_heatmap(
    hot_roc_result = hot_roc_result
  )

  # Check that hot_roc_result works
  formatted_hot_roc_result <- format_hot_roc_result(hot_roc_result)
  testthat::expect_equal(nrow(formatted_hot_roc_result), 24)
  testthat::expect_equal(ncol(formatted_hot_roc_result), 5)

  # Check ggplot object was successfully created
  testthat::expect_equal(class(p1)[1], 'gg')



})
