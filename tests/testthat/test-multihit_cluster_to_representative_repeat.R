test_that("multihit_cluster_to_representative_repeat works", {

  df_aavenger_mhc <- readRDS(testthat::test_path('testdata', 'example_aavenger_multihitcluster_table.rds'))

  repeat_granges <- readRDS(testthat::test_path('testdata', 'repeatclass_all.rds'))

  mhc_repeats_counts_per_cluster <- multihit_cluster_to_representative_repeat(aavenger_mhc = df_aavenger_mhc, repeat_table_grange = repeat_granges, repeat_column_name = 'repeat_class_subject')

  testthat::expect_equal(nrow(mhc_repeats_counts_per_cluster), 6)

})
