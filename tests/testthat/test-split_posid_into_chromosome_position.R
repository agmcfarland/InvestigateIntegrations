test_that("test split_posid_into_chromosome works", {

  df_aavenger_table <- read.csv(testthat::test_path('testdata', 'example_aavenger_table.csv'))

  df_test <- df_aavenger_table %>%
    split_posid_into_chromosome_position()

  testthat::expect_equal(nrow(df_aavenger_table), nrow(df_test))

  testthat::expect_equal(ncol(df_test), 19)

  testthat::expect_equal(class(df_test$position), 'numeric')

  testthat::expect_equal(class(df_test$extra), 'numeric')

  testthat::expect_equal(class(df_test$chromosome), 'character')

  testthat::expect_equal(class(df_test$strand), 'character')

})
