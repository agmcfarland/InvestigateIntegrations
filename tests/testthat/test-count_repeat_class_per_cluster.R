test_that("count_repeat_class_per_cluster works", {

  df_aavenger_mhc <- readRDS(testthat::test_path('testdata', 'example_aavenger_multihitcluster_table.rds'))

  repeat_granges <- readRDS(testthat::test_path('testdata', 'repeatclass_all.rds'))

  overlap_with_repeat_results <- find_overlaps_in_repeats(aavenger_table = df_aavenger_mhc, repeat_table_grange = repeat_granges, site_type = 'multihits')

  count_results <- count_repeat_class_per_cluster(aavenger_mhc_repeat_overlaps = overlap_with_repeat_results, repeat_column_name = 'repeat_class_subject')

  testthat::expect_equal(nrow(count_results), 16)

  testthat::expect_error(count_repeat_class_per_cluster(aavenger_mhc_repeat_overlaps = overlap_with_repeat_results, repeat_column_name = 'repeat_class_subjects'))

  overlap_with_repeat_results <- overlap_with_repeat_results %>%
    dplyr::rename(repeat_class = repeat_class_subject)

  count_results2 <- count_repeat_class_per_cluster(aavenger_mhc_repeat_overlaps = overlap_with_repeat_results, repeat_column_name = 'repeat_class')

  testthat::expect_equal(nrow(count_results2), 16)

  testthat::expect_equal(count_results, count_results2)

  overlap_with_repeat_results <- overlap_with_repeat_results %>%
    dplyr::rename(repeat_class_simplified = repeat_class)

  count_results3 <- count_repeat_class_per_cluster(aavenger_mhc_repeat_overlaps = overlap_with_repeat_results, repeat_column_name = 'repeat_class_simplified')

  testthat::expect_equal(count_results, count_results3)

})
