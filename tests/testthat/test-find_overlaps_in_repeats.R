test_that("find_overlaps_in_repeats works", {

  df_aavenger_mhc <- readRDS(testthat::test_path('testdata', 'example_aavenger_multihitcluster_table.rds'))

  repeat_granges <- readRDS(testthat::test_path('testdata', 'repeatclass_all.rds'))

  overlap_with_repeat_results <- find_overlaps_in_repeats(aavenger_table = df_aavenger_mhc, repeat_table_grange = repeat_granges, site_type = 'multihits')

  testthat::expect_equal(
    nrow(overlap_with_repeat_results),
    df_aavenger_mhc %>%
      unnest_mhc_table() %>%
      nrow() + 1 # edit this if test dataset changes. Found one site with two hits in the repeat table.
  )

  df_aavenger_unique_sites <- read.csv(testthat::test_path('testdata', 'example_aavenger_table.csv'))

  overlap_with_repeat_results <- find_overlaps_in_repeats(aavenger_table = df_aavenger_unique_sites, repeat_table_grange = repeat_granges, site_type = 'unique')

  testthat::expect_equal(nrow(overlap_with_repeat_results), 2002)

})
