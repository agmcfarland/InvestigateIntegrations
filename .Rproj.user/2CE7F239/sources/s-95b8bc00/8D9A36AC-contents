testthat::test_that('Format AAVengeR works', {

  aavenger_table <- read.csv(testthat::test_path('testdata', 'example_aavenger_table.csv'))

  formatted_aavenger_table <- format_aavenger_sites(
    aavenger_sites_df = aavenger_table
  )
  testthat::expect_equal(nrow(aavenger_table), nrow(formatted_aavenger_table))
})
