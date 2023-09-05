
rm(list = ls())

library(ShortRead)
library(dplyr)

# hg38 genome generated using the following:

#  mkdir /home/ubuntu/temp_data/data
#  cd /home/ubuntu/temp_data/data
#    wget --timestamping 'ftp://hgdownload.cse.ucsc.edu/goldenPath/hg38/chromosomes/*'
#    # remove alts, random, un, and readme. using full filepath for extra safety.
#    rm /home/ubuntu/temp_data/data/*random.fa.gz
#    rm /home/ubuntu/temp_data/data/*alt.fa.gz
#    rm /home/ubuntu/temp_data/data/chrUn_*.fa.gz
#    rm /home/ubuntu/temp_data/data/README*
#    rm /home/ubuntu/temp_data/data/chrM*
#    cat *.fa.gz > hg38.fa.gz

package_dir <- '/data/InvestigateIntegrations'

genome_fasta <- ShortRead::readFasta(file.path('/home/ubuntu/temp_data/data','hg38.fa.gz'))

# non-function version of IntegrationFeatureHeatmap::fasta_to_chromosome_lengths()
example_hg38_chromosome_lengths <- data.frame(
  'seqname' = ShortRead::id(genome_fasta),
  'chr_length' = ShortRead::width(genome_fasta)
)%>%
  dplyr::arrange(desc(chr_length))

write.csv(example_hg38_chromosome_lengths, file.path(package_dir, 'tests', 'testthat', 'testdata', 'example_hg38_chromosome_lengths.csv'), row.names = FALSE)

usethis::use_data(example_hg38_chromosome_lengths, overwrite = TRUE)
