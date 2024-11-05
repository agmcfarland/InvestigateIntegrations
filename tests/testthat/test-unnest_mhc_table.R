test_that("unnest_mhc_table returns ", {

  df_aavenger_mhc <- readRDS(testthat::test_path('testdata', 'example_aavenger_multihitcluster_table.rds'))

  df_multihit_cluster <- unnest_mhc_table(df_aavenger_mhc)

  testthat::expect_equal(
    length(df_aavenger_mhc$posids[[1]]),
    df_multihit_cluster %>% dplyr::filter(sample == 'sample1-2', clusterID == 'MHC.1') %>% dplyr::pull(posid) %>% length()
    )

})
