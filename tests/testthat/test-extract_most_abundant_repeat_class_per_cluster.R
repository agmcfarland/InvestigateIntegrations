test_that("extract_most_abundant_repeat_class_per_cluster works", {

  df_aavenger_mhc <- readRDS(testthat::test_path('testdata', 'example_aavenger_multihitcluster_table.rds'))

  repeat_granges <- readRDS(testthat::test_path('testdata', 'repeatclass_all.rds'))

  overlap_with_repeat_results <- find_overlaps_in_repeats(aavenger_table = df_aavenger_mhc, repeat_table_grange = repeat_granges, site_type = 'multihits')

  count_results <- count_repeat_class_per_cluster(aavenger_mhc_repeat_overlaps = overlap_with_repeat_results)

  mhc_repeats_counts_per_cluster <- extract_most_abundant_repeat_class_per_cluster(count_results)

  testthat::expect_equal(nrow(mhc_repeats_counts_per_cluster), 6)

})
