test_that("extend_raw_refseq_table works", {
  rm(list = ls())

  load(testthat::test_path('testdata', 'example_refseq_full_table.rda'))

  # test small reference set identical to separate_gene_features
  test_reference <- data.frame(
    'name' = c('XR_001750861.2', 'NM_001419809.1', 'NM_004739.4', 'XM_047426116.1', 'NR_107046.1'),
    'name2' = c('LOC107984663', 'KDM6A', 'MTA2', 'LOC124902420', 'MIR8079'),
    'strand' = c('-', '+', '-', '-', '-'),
    'type' = c('none', 'cmpl', 'cmpl', 'cmpl', 'none'),
    'left_UTR_length' = c(NA, 364, 661, 271, NA),
    'right_UTR_length' = c(NA, 1368, 415, 108, NA)
  )

  df_temp <- example_refseq_full_table %>%
    dplyr::filter(name2 %in% test_reference$name2)

  df_temp2 <- extend_raw_refseq_table(refseq_table = df_temp, number_of_cores = parallel::detectCores())

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

  # test larger dataset. The full example_refseq_full_table and an added test_reference_2 table that
  # has cases where introns < 0 if not corrected

  test_reference_2 <- data.frame(
    bin = c(900, 1307),
    name = c("NM_001291281.3", "NM_015068.3"),
    chrom = c("chr1", "chr7"),
    strand = c("+", "+"),
    txStart = c(41361433, 94656324),
    txEnd = c(41383590, 94669695),
    cdsStart = c(41361930, 94663556),
    cdsEnd = c(41382681, 94665682),
    exonCount = c(3, 3),
    exonStarts = c("41361433,41381615,41382209,", "94656324,94663333,94664513,"),
    exonEnds = c("41362344,41382208,41383590,", "94656580,94664513,94669695,"),
    score = c(0, 0),
    name2 = c("FOXO6", "PEG10"),
    cdsStartStat = c("cmpl", "cmpl"),
    cdsEndStat = c("cmpl", "cmpl"),
    exonFrames = c("0,0,2,", "-1,0,1,")
  )
  example_refseq_full_table_extra <- rbind(example_refseq_full_table, test_reference_2)

  df_temp4 <- extend_raw_refseq_table(refseq_table = example_refseq_full_table_extra, number_of_cores = parallel::detectCores())

  df_temp5 <- example_refseq_full_table_extra %>%
    dplyr::select(name, exonCount, cdsStartStat)

  testthat::expect_equal(c('NM_001291281.3', 'NM_015068.3') %in% df_temp4$name, c(TRUE, TRUE))

  df_temp5 <- merge(df_temp5, df_temp4, by = 'name')

  # test protein coding sequences
  df_temp6 <- df_temp5 %>%
    dplyr::filter(cdsStartStat == 'cmpl')

  lapply(dplyr::group_split(df_temp6, name), function (gene_group) {

    gene_group <- gene_group %>%
      dplyr::arrange(start, end)

    df_min_end <- gene_group %>%
      dplyr::filter(start == min(start))

    testthat::expect_equal('UTR_left' %in% df_min_end$feature, TRUE)

    df_max_end <- gene_group %>%
      dplyr::filter(end == max(end))
    testthat::expect_equal('UTR_right' %in% df_max_end$feature, TRUE)

    })

  testthat::expect_equal(nrow(df_temp5), 23764)

  df_temp7 <- df_temp5

  lapply(dplyr::group_split(df_temp7, name), function (gene_group) {

    number_of_exons_truth <- gene_group$exonCount[1]

    number_of_exons_calculated <- sum(gene_group$feature == 'exon')

    number_of_introns_calculated <- sum(gene_group$feature == 'intron')

    testthat::expect_equal(number_of_exons_calculated, number_of_exons_truth)

    })



  # # nrow(df_temp4)
  #
  # full_example <- read.csv(file.path('/data/temp/ncbiRefSeq.txt'), sep = '\t', header = FALSE)
  # colnames(full_example) <- c(
  #   'bin', 'name', 'chrom', 'strand', 'txStart', 'txEnd', 'cdsStart', 'cdsEnd', 'exonCount',
  #   'exonStarts', 'exonEnds', 'score', 'name2', 'cdsStartStat', 'cdsEndStat', 'exonFrames'
  # )
  # full_example <- full_example %>%
  #   dplyr::filter(
  #     !stringr::str_detect(chrom, '_alt'),
  #     !stringr::str_detect(chrom, '_random'),
  #     !stringr::str_detect(chrom, 'chrUn'),
  #     !stringr::str_detect(chrom, '_fix')) %>%
  #   base::unique()
  #
  # start.time <- Sys.time()
  # df_temp5 <- extend_raw_refseq_table(refseq_table = full_example, number_of_cores = parallel::detectCores())
  # print(Sys.time() - start.time)
  #
  # df_temp5 <- df_temp5 %>%
  #   dplyr::mutate(difference = end-start)
  #
  # df_temp6 <- df_temp5 %>%
  #   dplyr::filter(name == 'NM_001291281.3') %>%
  #   dplyr::arrange(start, end)
  #
  # df_temp8 <- df_temp5 %>%
  #   dplyr::filter(name == 'NM_015068.3') %>%
  #   dplyr::arrange(start, end)
  #
  # df_temp7 <- df_temp5 %>%
  #   dplyr::filter(difference < 0)
  #
  # nrow(df_temp5)
  #
  # print(full_example %>% dplyr::filter(name %in% c('NM_001291281.3', 'NM_015068.3')))



  # NM_001419809.1
  # KDM6A
  # https://genome.ucsc.edu/cgi-bin/hgc?hgsid=2210204836_nSdmpxWughtaaubaLVAyb5Aw51dK&g=htcCdnaAli&i=NM_001419809.1&c=chrX&l=44873187&r=45112779&o=44873187&aliTable=ncbiRefSeqPsl
  # plus strand
  # 5' UTR is 364 bp long (LEFT)
  # 3' UTR is 1368 bp long (RIGHT)

  # NM_004739.4
  # MTA2
  # https://genome.ucsc.edu/cgi-bin/hgc?hgsid=2210204836_nSdmpxWughtaaubaLVAyb5Aw51dK&g=htcCdnaAliInWindow&i=NM_004739.4&c=chr11&l=62593213&r=62601865&o=62593213&aliTable=ncbiRefSeqPsl&table=ncbiRefSeqCurated
  # minus strand
  # 5' UTR is 415 (RIGHT)
  # 3' UTR is 661 (LEFT)

  # XR_001750861.2
  # LOC107984663
  # No UTR (is RNA)

  # XM_047426116.1
  # LOC124902420
  # https://genome.ucsc.edu/cgi-bin/hgc?hgsid=2219552494_L4xDP0MO95nnBvZRSD8IlxfAd8eR&g=htcCdnaAli&i=XM_047426116.1&c=chr10&l=47416099&r=47418455&o=47416099&aliTable=ncbiRefSeqPsl
  # Single exon
  # minus strand
  # 5' UTR is 271 (RIGHT)
  # 3' UTR is 108 (LEFT)

})
