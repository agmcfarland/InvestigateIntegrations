# This is a linear record of commands used to build this package.

rm(list = ls())

library(devtools)

package_dir <- '/data/InvestigateIntegrations'

devtools::load_all(".")

usethis::create_package(package_dir)
usethis::use_mit_license('Alexander G. McFarland')
usethis::use_description(fields = list(
  "Authors@R" = utils::person(
    "Alexander", "McFarland",
    email = "alex.925@gmail.com",
    role = c("aut", "cre"),
    comment = c(ORCID = "0000-0002-1803-3623")
  ),
  Version = '1.3',
  Title = 'Invetigate integrations in genomic features',
  URL = 'https://github.com/agmcfarland/InvestigateIntegrations',
  BugReports = 'https://github.com/agmcfarland/InvestigateIntegrations/issues',
  Language =  "en",
  License = "MIT + file LICENSE",
  Description = 'Functions and workflows to compare integrations in different genomic features.',
  Encoding = 'UTF-8',
  LazyData = 'true',
  LazyDataCompression = 'xz',
  biocViews = '' #https://github.com/egeulgen/pathfindR/issues/14
))

usethis::use_build_ignore(c('docs', 'create'))

# Create "create" to store all project creation instructions including this script
dir.create(file.path(package_dir, 'create'))
# Create docs to store documents
dir.create(file.path(package_dir, 'docs'))

# Create test folder with usethis, create testdata dir, and then remove test-whoop.R create initially
# usethis::use_test('whoop')
# dir.create(file.path(package_dir, 'tests', 'testthat', 'testdata'))
# file.remove(file.path(package_dir, 'tests', 'testthat', 'test-whoop.R'))

# Create example_hg38_chromosome_lengths dataset. Fill in with code. will save new file to testdata and data
usethis::use_data_raw(name = 'example_hg38_chromosome_lengths')
# Manually fill in documentation.
usethis::use_r(name = 'example_hg38_chromosome_lengths')

# Create an example aavenger table dataset
usethis::use_data_raw(name = 'example_aavenger_table')
# Manually fill in documentation.
usethis::use_r(name = 'example_aavenger_table')

# Create an example RData features file
usethis::use_data_raw(name = 'example_H3K79me2_rdata')
# Manually fill in documentation.
usethis::use_r(name = 'example_H3K79me2_rdata')

# Create an example rds features file
usethis::use_data_raw(name = 'example_H3K79me2_rds')
# Manually fill in documentation.
usethis::use_r(name = 'example_H3K79me2_rds')

# Create another example rds features file
usethis::use_data_raw(name = 'example_refGene_rds')
# Manually fill in documentation.
usethis::use_r(name = 'example_refGene_rds')

# Create an example multihitcluster features. store in testdata
usethis::use_data_raw(name = 'example_aavenger_multihitcluster_table')
# Manually fill in documentation.
usethis::use_r(name = 'example_aavenger_multihitcluster_table')

# Create an example repeat granges object. store in testdat.
usethis::use_data_raw(name = 'example_repeats_all')
# Manually fill in documentation.
usethis::use_r(name = 'example_repeats_all')


# Package functions. Create each file and then write code.
function_name <- 'format_aavenger_sites'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)

function_name <- 'fasta_to_chromosome_lengths'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)

function_name <- 'sample_genome'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)

function_name <- 'aavenger_sites_random_match'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)

function_name <- 'test_for_overlaps'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)

function_name <- 'hotroc_compare_insertion_to_match'
usethis::use_r(function_name)
# usethis::use_test(function_name)
# devtools::test(filter = function_name)

function_name <- 'format_hot_roc_result'
usethis::use_r(function_name)
# usethis::use_test(function_name)
# devtools::test(filter = function_name)

function_name <- 'roc_to_heatmap'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)


# T-test workflow
function_name <- 'count_integrations_in_feature_window'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)

function_name <- 'simulate_many_counts_in_features'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)

function_name <- 'experimental_counts_in_features'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)

function_name <- 'rnorm_simulate_many_counts_in_features'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)

function_name <- 'integration_percentage_in_feature_p_value'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)

function_name <- 'plot_integration_percentages'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)

## Counting integration sites

# Create an example rds features file. Will create in raw-data and then finally in data
usethis::use_data_raw(name = 'example_refseq_full_table')
# Manually fill in documentation.
usethis::use_r(name = 'example_refseq_full_table')
# copy to testdata to use for testing functions
file.copy(
  from = file.path('/data/InvestigateIntegrations/data/example_refseq_full_table.rda'),
  to = file.path('/data/InvestigateIntegrations/tests/testthat/testdata')
  )

# Extend UCSC refseq data table to include introns, UTRs, exons using a parallel call to separate_gene_features
function_name <- 'extend_raw_refseq_table'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)

function_name <- 'separate_gene_features'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)

# Count integration sites in features
function_name <- 'extract_overlaps'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)

## GC content for each integration site
# Create an example rds features fasta file. Will create in raw-data and then finally in data
usethis::use_data_raw(name = 'example_fasta_chromosome')
# Manually fill in documentation.
usethis::use_r(name = 'example_fasta_chromosome')

function_name <- 'GC_percentage_per_insertion'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)


## Repeat analysis
function_name <- 'unnest_mhc_table'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)

function_name <- 'split_posid_into_chromosome_position'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)

function_name <- 'relabel_repeat_categories'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)

function_name <- 'find_overlaps_in_repeats'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)

function_name <- 'count_repeat_class_per_cluster'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)

function_name <- 'extract_most_abundant_repeat_class_per_cluster'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)

function_name <- 'multihit_cluster_to_representative_repeat'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)

function_name <- 'repeat_class_summary_tables'
usethis::use_r(function_name)
usethis::use_test(function_name)
devtools::test(filter = function_name)

# Load functions and datasets
devtools::load_all(".")

## WHEN BUILDING FOR NEW RELEASE:

# update usethis::use_description() at top

# Run all tests
devtools::test()
devtools::test_coverage()

## Update DESCRIPTION file with imports from NAMESPACE
devtools::document() # updates NAMESPACE

#Depends:
#    R (>= 4.1.3)

usethis::use_dev_package("hotROCs", remote = "agmcfarland/hotROCs")

#Imports:
usethis::use_package("dplyr")
usethis::use_package("dplyr")
usethis::use_package("ggplot2")
usethis::use_package("stringr")
usethis::use_package("tibble")
usethis::use_package("tidyr")
usethis::use_package("truncnorm")
usethis::use_package("GenomicRanges")
usethis::use_package("ShortRead")
usethis::use_package("Biostrings")
usethis::use_package("parallel")
usethis::use_package("BSgenome")

