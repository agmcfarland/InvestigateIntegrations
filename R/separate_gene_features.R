#' Separate gene features
#'
#' This function separates gene features such as exons, introns, and UTRs from a gene entry.
#'
#' @param gene_entry A data frame row representing a single gene entry.
#' @return A data frame row containing separated gene features with columns start, end, feature, and name
#' @export
#' @import dplyr
#' @details Gene entry must have the following columns:
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
#'
#'
#' @examples
#' # Create a sample gene entry
#' gene_entry <- data.frame(
#'   exonStarts = "100,200,300",
#'   exonEnds = "150,250,350",
#'   exonCount = 3,
#'   cdsStartStat = "cmpl",
#'   txStart = 90,
#'   txEnd = 360,
#'   cdsStart = 120,
#'   cdsEnd = 330,
#'   name = "gene1"
#' )
#'
#' # Separate gene features
#' separated_features <- separate_gene_features(gene_entry)
#'
#' # View the separated features
#' print(separated_features)
#'
separate_gene_features <- function(gene_entry) {

  # gene_entry <- df_temp[6,]

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
    )
    df_gene_boundaries <- rbind(df_gene_boundaries, df_introns)
  }

  if (gene_entry$cdsStartStat == 'cmpl') {

    df_UTR <- data.frame(
      'start' = c(gene_entry$txStart, gene_entry$cdsEnd),
      'end' = c(gene_entry$txStart + (gene_entry$cdsStart - gene_entry$txStart), gene_entry$txEnd),
      'feature' = c('UTR_left', 'UTR_right')
    )

    df_gene_boundaries <- rbind(df_gene_boundaries, df_UTR)

    penultimate_row <- nrow(df_gene_boundaries) - 1

    # df_gene_boundaries <- df_gene_boundaries %>%
    #   dplyr::arrange(start, end) %>%
    #   dplyr::mutate(
    #     start = ifelse(feature == 'exon' & dplyr::row_number() == 2, dplyr::lag(end + 1, n = 1), start),
    #     end = ifelse(feature == 'exon' & dplyr::row_number() == penultimate_row, dplyr::lead(start - 1, n = 1), end)
    #   )
  }

  df_gene_boundaries$name <- gene_entry$name

  return(df_gene_boundaries)

}
