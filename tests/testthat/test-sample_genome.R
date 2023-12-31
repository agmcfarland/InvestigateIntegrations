testthat::test_that('sample genome works', {

  genome_fasta <- ShortRead::ShortRead(Biostrings::DNAStringSet(c('ACTGACTGACTGACTG','GGGGGGGGGGGGGGGGGGGG','CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC')))

  genome_fasta@id <- Biostrings::BStringSet(c('chr1','chr2','chr3'))

  chromosome_lengths <- fasta_to_chromosome_lengths(genome_fasta = genome_fasta)

  output <- sample_genome(
    number_of_random_positions = 10,
    chromosome_lengths = chromosome_lengths,
    random_seed_value = 2)

  seqname_col <- c('chr1', 'chr1', 'chr1', 'chr2', 'chr2', 'chr3', 'chr3', 'chr3', 'chr3', 'chr3')

  random_positions_col <- c(13,9,2,11,1,3,16,32,8,39)

  testthat::expect_equal(output$seqname, seqname_col)
  testthat::expect_equal(output$random_positions, random_positions_col)

})




