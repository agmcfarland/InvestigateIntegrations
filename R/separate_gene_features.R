#' Separate gene features
#'
#' This function separates gene features such as exons, introns, and UTRs from a gene entry.
#'
#' @param gene_entry A data frame row representing a single gene entry.
#' @return A data frame row containing separated gene features with columns start, end, feature, and name
#' @export
#' @import dplyr
#' @details Gene entry must have the following columns:
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
#'
#'   Returns a dataframe with each row being a range for the category of UTR_left, UTR_right, exon, and intron
separate_gene_features <- function(gene_entry) {

  # gene_entry <- test_reference_2[1,]

  df_gene_boundaries <- data.frame(
    'start' = as.numeric(unlist(strsplit(gene_entry$exonStarts, ',', fixed = TRUE))),
    'end' = as.numeric(unlist(strsplit(gene_entry$exonEnds, ',', fixed = TRUE))),
    'feature' = 'exon'
  )

  if (gene_entry$exonCount > 1) {

    end_introns <- df_gene_boundaries$start - 1

    df_introns <- data.frame(
      'start' =  dplyr::lag(df_gene_boundaries$end + 1, n = 1)[2:gene_entry$exonCount],
      'end' = end_introns[2:gene_entry$exonCount],
      'feature' = 'intron'
    ) %>%
      dplyr::filter(end > start)


    df_gene_boundaries <- rbind(df_gene_boundaries, df_introns)
  }

  if (gene_entry$cdsStartStat == 'cmpl') {

    df_UTR <- data.frame(
      'start' = c(gene_entry$txStart, gene_entry$cdsEnd),
      'end' = c(gene_entry$txStart + (gene_entry$cdsStart - gene_entry$txStart), gene_entry$txEnd),
      'feature' = c('UTR_left', 'UTR_right')
    )

    df_gene_boundaries <- rbind(df_gene_boundaries, df_UTR)


  }

  df_gene_boundaries$name <- gene_entry$name
  df_gene_boundaries$chrom <- gene_entry$chrom
  df_gene_boundaries$strand <- gene_entry$strand

  return(df_gene_boundaries)

}
