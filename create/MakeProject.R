rm(list = ls())

library(devtools)

package_dir <- '/data/InvestigateIntegrations'

usethis::create_package(package_dir)
usethis::use_mit_license('Alexander G. McFarland')
usethis::use_description(fields = list(
  "Authors@R" = utils::person(
    "Alexander", "McFarland",
    email = "alex.925@gmail.com",
    role = c("aut", "cre"),
    comment = c(ORCID = "0000-0002-1803-3623")
  ),
  Version = '1.0',
  Title = 'Invetigate integrations in genomic features.',
  URL = 'https://github.com/agmcfarland/InvestigateIntegrations',
  BugReports = 'https://github.com/agmcfarland/InvestigateIntegrations/issues',
  Language =  "en",
  License = "MIT + file LICENSE",
  Description = 'Functions and workflows to compare integrations in different genomic features.',
  Encoding = 'UTF-8',
  LazyData = 'true',
  LazyDataCompression = 'xz'
))

usethis::use_build_ignore(c('docs', 'create'))

# Create raw-data to store all project creation instructions including this script
dir.create(file.path(package_dir, 'create'))
# Create docs to store documents
dir.create(file.path(package_dir, 'docs'))

# Create test folder with usethis, create testdata dir, and then remove test-whoop.R create initially
usethis::use_test('whoop')
dir.create(file.path(package_dir, 'tests', 'testthat', 'testdata'))
file.remove(file.path(package_dir, 'tests', 'testthat', 'test-whoop.R'))

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


# Z-test workflow
function_name <- 'roc_to_heatmap'


# Run all tests
devtools::test()
devtools::test_coverage()

devtools::document() # updates NAMESPACE


