test_that("repeat_class_summary_tables works", {

  df_aavenger_mhc <- readRDS(testthat::test_path('testdata', 'example_aavenger_multihitcluster_table.rds'))

  repeat_granges <- readRDS(testthat::test_path('testdata', 'repeatclass_all.rds'))

  mhc_repeats_counts_per_cluster <- multihit_cluster_to_representative_repeat(aavenger_mhc = df_aavenger_mhc, repeat_table_grange = repeat_granges, repeat_column_name = 'repeat_class_subject') %>%
    dplyr::mutate(
      subject = stringr::str_replace(subject, 'sample', 'subject'),
      subject = ifelse(stringr::str_detect(sample, 'sample1-2'), 'subject2', subject),
      sample = 'sample1',
      trial = 'testset'
      )
  df_aavenger_unique_sites <- read.csv(testthat::test_path('testdata', 'example_aavenger_table.csv'))

  overlap_with_repeat_results <- find_overlaps_in_repeats(aavenger_table = df_aavenger_unique_sites, repeat_table_grange = repeat_granges, site_type = 'unique') %>%
    dplyr::select(dplyr::matches(paste0("_query|", 'repeat_class'))) %>%
    dplyr::rename(repeat_class = repeat_class_subject)

  colnames(overlap_with_repeat_results) <- stringr::str_replace_all(colnames(overlap_with_repeat_results), '_query', '')

  overlap_with_repeat_results <- overlap_with_repeat_results %>%
    relabel_repeat_categories() %>%
    dplyr::mutate(repeat_class = repeat_simplified)

  repeat_class_summary_test <- repeat_class_summary_tables(aavenger_unique_sites_table = overlap_with_repeat_results, mhc_repeats_counts_per_cluster = mhc_repeats_counts_per_cluster)

  testthat::expect_equal(length(repeat_class_summary_test), 3)

  testthat::expect_equal(nrow(repeat_class_summary_test[['unique']]),  19)

  testthat::expect_equal(nrow(repeat_class_summary_test[['multihits']]),  3)

  testthat::expect_equal(nrow(repeat_class_summary_test[['combined']]),  19)

  repeat_class_summary_test2 <- repeat_class_summary_tables(aavenger_unique_sites_table = overlap_with_repeat_results, mhc_repeats_counts_per_cluster = mhc_repeats_counts_per_cluster %>%
                                                              dplyr::mutate(repeat_class = paste0(seq(1, n()), '_fake_repeat')))

  testthat::expect_equal(nrow(repeat_class_summary_test2[['unique']]),  19)

  testthat::expect_equal(nrow(repeat_class_summary_test2[['multihits']]),  6)

  testthat::expect_equal(nrow(repeat_class_summary_test2[['combined']]),  25)

})
