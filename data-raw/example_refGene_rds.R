## code to prepare `refGene.rds` dataset goes here
example_refGene_rds <- readRDS('/data/InvestigateIntegrations/tests/testthat/testdata/refGene.rds')

usethis::use_data(example_refGene_rds, overwrite = TRUE)
