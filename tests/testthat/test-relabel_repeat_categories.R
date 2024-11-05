test_that("test relabel_repeat_categories works", {

  # Creating the dataframe
  df_repeat_table_test <- data.frame(
    SW_score = c(1700, 232, 1700, 1700, 599, 959),
    percent_div = c(23.3, 16.5, 23.3, 23.3, 5.8, 6.8),
    percent_del = c(0.2, 7.6, 0.2, 0.2, 2.3, 2.3),
    percent_ins = c(0.1, 2.9, 0.1, 0.1, 0, 0),
    query_seq = c("chr20", "chr20", "chr20", "chr20", "chr20", "chr20"),
    query_start = c(60221, 62017, 62150, 63841, 67780, 67866),
    query_end = c(62014, 62147, 63215, 67292, 67865, 67998),
    query_after = c("(64382153)", "(64382020)", "(64380952)", "(64376875)", "(64376302)", "(64376169)"),
    strand = c("+", "+", "+", "+", "C", "+"),
    repeat_name = c("(ATTCC)n", "HSATII", "(ATTCC)n", "(ATTCC)n", "L1P4", "L1PA13"),
    repeat_class = c("Satellite/centr", "DNA/MULE-MuDR", "LTR/DIRS", "snRNA", "SINE/tRNA-RTE", "Retroposon/SVA"),
    repeat_start = c("2", "1", "2", "2", "(402)", "6015"),
    repeat_end = c(1796, 137, 1070, 3456, 5772, 6150),
    repeat_after = c("(0)", "(33)", "(0)", "(0)", "5685", "(13)"),
    ID = c(1, 2, 3, 4, 5, 6),
    alt = c(NA, NA, NA, NA, NA, NA)
  )

  df_test <- df_repeat_table_test %>%
    relabel_repeat_categories()

  testthat::expect_equal(df_test$repeat_simplified, c("satellite", "DNA", "LTR", "RNA", "SINE", "retrotransposon"))

  })
