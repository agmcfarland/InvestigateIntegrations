testthat::test_that("aavenger_sites_random_match works", {

  example_aavenger_table <- read.csv(testthat::test_path('testdata', 'example_aavenger_table.csv'))

  example_hg38_chromosome_lengths <- read.csv(testthat::test_path('testdata', 'example_hg38_chromosome_lengths.csv'))

  formatted_example_aavenger_table <- format_aavenger_sites(example_aavenger_table)

  match_row_number_modifier_ <- 3

  random_match_table <- aavenger_sites_random_match(
    aavenger_sites = formatted_example_aavenger_table,
    chromosome_lengths = example_hg38_chromosome_lengths,
    random_seed_value = 10,
    match_row_number_modifier = match_row_number_modifier_)

  number_of_heatmap_groups <- length(unique(formatted_example_aavenger_table$heatmap_group))

  expected_total_number_of_rows <- (nrow(example_aavenger_table)/number_of_heatmap_groups) * match_row_number_modifier_ * number_of_heatmap_groups

  testthat::expect_equal(nrow(random_match_table), expected_total_number_of_rows)

})
