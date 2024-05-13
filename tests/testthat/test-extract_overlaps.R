test_that("extract_overlaps works", {


  rm(list = ls())

  library(dplyr)


  test_query_table <- data.frame(
    seqname = c('chr16', 'chr3', 'chr2', 'chr16', 'chr16', 'chrX', 'chr3'),
    start = c(1000, 1000, 1000, 1000, 1000, 1000, 10000000),
    end = c(1000, 1000, 1000, 1000, 1000, 1000, 10000000),
    strand = c('*', '*', '*', '*', '*', '*', '*'),
    extra_info = c('treatment1', 'treatment1', 'treatment1', 'treatment2', 'treatment2', 'treatment2', 'treatment3')
  ) %>%
    GenomicRanges::makeGRangesFromDataFrame(keep.extra.columns = TRUE)

  test_subject_table <- data.frame(
    seqname = c('chr1', 'chr2', 'chr3', 'chr3', 'chr16', 'chrX', 'chr9'),
    start = c(0, 0, 0, 0, 0, 0, 0),
    end = c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
    strand = c('*', '*', '*', '*', '*', '*', '*'),
    extra_info = c('gene1', 'gene2', 'gene3', 'gene3_alt', 'gene4', 'gene5', 'gene6')
  ) %>%
    GenomicRanges::makeGRangesFromDataFrame(keep.extra.columns = TRUE)

  test_results <- extract_overlaps(
    query = test_query_table,
    subject = test_subject_table,
    mark_query = TRUE,
    mark_subject = TRUE
  )

  # Find 7 of 8 expected results
  testthat::expect_equal(nrow(test_results), 7)

  # Find same query found twice in subject
  two_separate_hits_same_query <- test_results %>%
    dplyr::filter(seqnames_query == 'chr3')

  testthat::expect_equal(nrow(two_separate_hits_same_query), 2)

  testthat::expect_equal(c('gene3', 'gene3_alt') %in% two_separate_hits_same_query$extra_info_subject, c(TRUE, TRUE))

  # Check treatment3 is correctly missing in subject
  testthat::expect_equal('treatment3' %in% test_results$extra_info_subject, FALSE)

  # Check gene4 is found in 3 separate queries
  three_gene4_hits <- test_results %>%
    dplyr::filter(seqnames_query == 'chr16')

  testthat::expect_equal(nrow(three_gene4_hits), 3)

  testthat::expect_equal(c('gene4') %in% three_gene4_hits$extra_info_subject, TRUE)

  testthat::expect_equal(c('treatment1', 'treatment2') %in% three_gene4_hits$extra_info_query, c(TRUE, TRUE))


  # Use larger test dataset

  load(testthat::test_path('testdata', 'example_refseq_full_table.rda'))

  example_aavenger_table <- read.csv(testthat::test_path('testdata', 'example_aavenger_table.csv')) %>%
    format_aavenger_sites() %>%
    GenomicRanges::makeGRangesFromDataFrame(keep.extra.columns = TRUE)

  granges_example_refseq <- example_refseq_full_table %>%
    dplyr::select(name, chrom, txStart, txEnd) %>%
    dplyr::rename(
      start = txStart,
      end = txEnd) %>%
    GenomicRanges::makeGRangesFromDataFrame(keep.extra.columns = TRUE)

  test_results <- extract_overlaps(
    query = example_aavenger_table,
    subject = granges_example_refseq,
    mark_query = TRUE,
    mark_subject = TRUE
  )

  testthat::expect_equal(nrow(test_results), 54)






})
