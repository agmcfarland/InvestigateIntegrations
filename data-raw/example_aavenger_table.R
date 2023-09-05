
rm(list = ls())

library(dplyr)

package_dir <- '/data/InvestigateIntegrations'

load('/home/ubuntu/temp_data/data/H3K79me2.RData')

set.seed(30)

number_of_sites <- 1000

example_aavenger_table <- rbind(data.frame(epigenData)%>%
                         dplyr::slice_sample(n = number_of_sites, replace = FALSE)%>%
                         dplyr::rowwise()%>%
                         dplyr::mutate(
                           trial = 'testset',
                           subject = 'subject1',
                           sample = 'sample1',
                           refGenome = 'hg38',
                           posid = ifelse(is.integer(start/2) == TRUE, paste0(seqnames,'+',start,'.1'), paste0(seqnames,'-',start,'.1')),
                           UMIs = 10,
                           sonicLengths = 10,
                           nRepsObs = 10,
                           flags = '',
                           vectorFastaFile = 'random.fasta',
                           `rep1-UMIs` = 10,
                           `rep1-sonicLengths` = 10,
                           `rep1-reads` = 10,
                           `rep1-repLeaderSeq` = 'ACTGACTG',
                           maxLeaderSeqDist = 10
                         ),
                       data.frame(epigenData)%>%
                         dplyr::slice_sample(n = number_of_sites, replace = FALSE)%>%
                         dplyr::rowwise()%>%
                         dplyr::mutate(
                           trial = 'testset',
                           subject = 'subject2',
                           sample = 'sample1',
                           refGenome = 'hg38',
                           posid = ifelse(is.integer(start/2) == TRUE, paste0(seqnames,'+',start,'.1'), paste0(seqnames,'-',start,'.1')),
                           UMIs = 10,
                           sonicLengths = 10,
                           nRepsObs = 10,
                           flags = '',
                           vectorFastaFile = 'random.fasta',
                           `rep1-UMIs` = 10,
                           `rep1-sonicLengths` = 10,
                           `rep1-reads` = 10,
                           `rep1-repLeaderSeq` = 'ACTGACTG',
                           maxLeaderSeqDist = 10
                         )

)
example_aavenger_table <- example_aavenger_table%>%
  dplyr::select(-c(seqnames, start, end, width, strand, mid))

write.csv(example_aavenger_table, file.path(package_dir, 'tests', 'testthat', 'testdata', 'example_aavenger_table.csv'), row.names = FALSE)

usethis::use_data(example_aavenger_table, overwrite = TRUE)
