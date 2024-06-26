test_that("GC_percentage_per_insertion works", {
  # rm(list = ls())
  # devtools::load_all(".")

  # library(dplyr)

  set.seed(123)

  df_insertion <- data.frame(
    seqname = rep('chr1', 5),
    start = sample.int(n = 1000, size = 5, replace = TRUE),
    width = 1 ,
    strand = '*',
    type = 'insertion',
    heatmap_group = 'testset_subject1_sample1') %>%
    dplyr::mutate(
      end = start,
      mid = start
    )

  df_match <- data.frame(
    seqname = rep('chr2', 15),
    start = sample.int(n = 1000, size = 15, replace = TRUE),
    width = 1 ,
    strand = '*',
    type = 'match',
    heatmap_group = 'testset_subject1_sample1') %>%
    dplyr::mutate(
      end = start,
      mid = start
    )

  set.seed(123)

  df_insertion2 <- data.frame(
    seqname = rep('chr1', 5),
    start = sample.int(n = 1000, size = 5, replace = TRUE),
    width = 1 ,
    strand = '*',
    type = 'insertion',
    heatmap_group = 'testset_subject1_sample2') %>%
    dplyr::mutate(
      end = start,
      mid = start
    )

  df_match2 <- data.frame(
    seqname = rep('chr2', 15),
    start = sample.int(n = 1000, size = 15, replace = TRUE),
    width = 1 ,
    strand = '*',
    type = 'match',
    heatmap_group = 'testset_subject1_sample2') %>%
    dplyr::mutate(
      end = start,
      mid = start
    )

  df_combined <- rbind(df_insertion, df_match, df_insertion2, df_match2)

  window_sizes_to_test <- c(100, 200, 300)

  genomic_sequence <- example_fasta_chromosome

  chromosome_lengths_ <- data.frame(genomic_sequence@ranges) %>%
    dplyr::select(width, names) %>%
    dplyr::rename(
      seqname = names,
      chr_length = width)

  chromosome_lengths <- chromosome_lengths_

  result <- GC_percentage_per_insertion(
    df_combined = df_combined,
    genomic_sequence = genomic_sequence,
    window_sizes_to_test = window_sizes_to_test,
    chromosome_lengths = chromosome_lengths_,
    fasta_name = 'genome'
  )

  testthat::expect_equal(nrow(result), 40)

  testthat::expect_equal(ncol(result), 11)

  # check ordering is correct and that correct results are generated twice in a row

  bp_100 <- c(0.5,
              0.5154639,
              0.6185567,
              0.5051546,
              0.622449,
              0.6315789,
              0.6122449,
              0.5638298,
              0.6774194,
              0.6262626,
              0.6326531,
              0.6666667,
              0.6122449,
              0.625,
              0.65625,
              0.6666667,
              0.7070707,
              0.7070707,
              0.6170213,
              0.6666667,
              0.5,
              0.5154639,
              0.6185567,
              0.5051546,
              0.622449,
              0.6315789,
              0.6122449,
              0.5638298,
              0.6774194,
              0.6262626,
              0.6326531,
              0.6666667,
              0.6122449,
              0.625,
              0.65625,
              0.6666667,
              0.7070707,
              0.7070707,
              0.6170213,
              0.6666667)

  testthat::expect_equal(round(result$genome_gc.fasta.100, 3), round(bp_100, 3))

  bp_200 <- c(0.513369,
              0.4947368,
              0.5625,
              0.4712042,
              0.5440415,
              0.6203209,
              0.640625,
              0.609375,
              0.625,
              0.6197917,
              0.6302083,
              0.6363636,
              0.6145833,
              0.6891192,
              0.6528497,
              0.6528497,
              0.6530612,
              0.6701031,
              0.6153846,
              0.6363636,
              0.513369,
              0.4947368,
              0.5625,
              0.4712042,
              0.5440415,
              0.6203209,
              0.640625,
              0.609375,
              0.625,
              0.6197917,
              0.6302083,
              0.6363636,
              0.6145833,
              0.6891192,
              0.6528497,
              0.6528497,
              0.6530612,
              0.6701031,
              0.6153846,
              0.6363636)

  testthat::expect_equal(round(result$genome_gc.fasta.200, 3), round(bp_200, 3))

  bp_300 <- c(0.4805654,
              0.4982332,
              0.5086505,
              0.4615385,
              0.5190311,
              0.6289753,
              0.6398601,
              0.6177606,
              0.6185567,
              0.6245614,
              0.6175439,
              0.6025641,
              0.6275862,
              0.6701031,
              0.6563574,
              0.6551724,
              0.641115,
              0.6666667,
              0.6034483,
              0.6254417,
              0.4805654,
              0.4982332,
              0.5086505,
              0.4615385,
              0.5190311,
              0.6289753,
              0.6398601,
              0.6177606,
              0.6185567,
              0.6245614,
              0.6175439,
              0.6025641,
              0.6275862,
              0.6701031,
              0.6563574,
              0.6551724,
              0.641115,
              0.6666667,
              0.6034483,
              0.6254417)

  testthat::expect_equal(round(result$genome_gc.fasta.300, 3), round(bp_300, 3))

  })
