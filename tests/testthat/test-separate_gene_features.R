test_that("separate_gene_features works", {
  rm(list = ls())

  load(testthat::test_path('testdata', 'example_refseq_full_table.rda'))

  test_reference <- data.frame(
    'name' = c('XR_001750861.2', 'NM_001419809.1', 'NM_004739.4', 'XM_047426116.1', 'NR_107046.1', 'NM_000168.6'),
    'name2' = c('LOC107984663', 'KDM6A', 'MTA2', 'LOC124902420', 'MIR8079', 'GLI3'),
    'strand' = c('-', '+', '-', '-', '-', '-'),
    'type' = c('none', 'cmpl', 'cmpl', 'cmpl', 'none', 'compl'),
    'left_UTR_length' = c(NA, 364, 661, 271, NA, 3381),
    'right_UTR_length' = c(NA, 1368, 415, 108, NA, 13956)
  )

  df_temp <- example_refseq_full_table %>%
    dplyr::filter(name2 %in% test_reference$name2)

  df_temp2 <- do.call(rbind, lapply(split(df_temp, 1:nrow(df_temp)), separate_gene_features))

  testthat::expect_equal(test_reference$name %in% base::unique(df_temp2$name), rep(TRUE, length(test_reference$name)))

  df_temp3 <- df_temp2 %>%
    dplyr::filter(stringr::str_detect(feature, 'UTR')) %>%
    dplyr::mutate(
      UTR_length = end - start
    ) %>%
    dplyr::select(-c(start, end)) %>%
    tidyr::pivot_wider(names_from = 'feature', values_from = 'UTR_length')

  df_temp3 <- merge(df_temp3, test_reference, by = 'name')

  testthat::expect_equal(df_temp3$UTR_left, df_temp3$left_UTR_length)

  testthat::expect_equal(df_temp3$UTR_right, df_temp3$right_UTR_length)
})
