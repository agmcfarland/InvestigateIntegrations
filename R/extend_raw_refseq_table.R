#' Extend raw RefSeq table
#'
#' This function extends a raw RefSeq table by splitting it into chunks based on the number of cores specified,
#' processing each chunk in parallel, and then combining the results into a single table.
#'
#' @param refseq_table A data frame containing raw RefSeq data.
#' @param number_of_cores The number of CPU cores to be used for parallel processing. Default is 8.
#' @return A data frame containing extended RefSeq data with columns start, end, feature, and name
#' @export
#' @import dplyr parallel
#'
#' @details Calls on InvestigateIntegrations::separate_gene_features()
#'   Dataframe must have the following columns:
#'   strand genomic strand
#'   chrom chromosome name
#'   exonStarts comma delimited list of exon positions and end with a comma (character)
#'   exonEnds comma delimited list of exon positions and end with a comma (character)
#'   exonCount number of exons in exonStarts/exonEnds (numeric)
#'   cdsStartStat either cmpl or none (character)
#'   txStart transcription start (numeric)
#'   txEnd transcription end (numeric)
#'   cdsStart coding start (numeric)
#'   cdsEnd coding end (numeric)
#'   name identifier
#'
#'   All numeric positions are being read in 5'->3'
#'   Returns a dataframe with each row being a range for the category of UTR_left, UTR_right, exon, and intron
extend_raw_refseq_table <- function(refseq_table, number_of_cores = 8) {

  gene_list <- split(refseq_table, 1:nrow(refseq_table))

  cl <- parallel::makeCluster(number_of_cores)

  parallel::clusterEvalQ(cl, library("dplyr"))

  parallel::clusterExport(cl, list('gene_list', 'separate_gene_features'), envir = base::environment())

  result <- parallel::parLapply(cl, gene_list, separate_gene_features)

  parallel::stopCluster(cl)

  return(do.call(rbind, result))

}
