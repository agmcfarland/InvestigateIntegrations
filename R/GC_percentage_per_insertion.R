#' Calculate GC Content Around Insertion Sites
#'
#' This function calculates the GC content for various window sizes around genomic insertion sites.
#' It takes a dataframe of insertion sites, a genomic sequence, and a set of window sizes, and
#' returns the original dataframe augmented with columns representing the GC content for each window size.
#'
#' @param df_combined A dataframe containing genomic insertion sites with at least the following columns:
#' \describe{
#'   \item{seqname}{Chromosome name or sequence identifier}
#'   \item{start}{Start position of the insertion}
#'   \item{end}{End position of the insertion}
#' }
#' @param genomic_sequence A `BSgenome` object containing the genomic sequence data.
#' @param window_sizes_to_test A numeric vector of window sizes (in base pairs) to test around each insertion site.
#' @param chromosome_lengths A dataframe with chromosome lengths, containing at least the following columns:
#' \describe{
#'   \item{seqname}{Chromosome name or sequence identifier}
#'   \item{chr_length}{Length of the chromosome}
#' }
#' @param fasta_name A string representing the base name for the output fasta file, default is 'genome'.
#'
#' @return A dataframe identical to `df_combined` but with additional columns for each window size tested.
#' Each new column represents the GC content calculated for that specific window size around the insertion sites.
#'
#' @import dplyr
#' @import GenomicRanges
#' @import Biostrings
#' @import tidyr
#' @import BSgenome
#' @export
GC_percentage_per_insertion <- function(
    df_combined,
    genomic_sequence,
    window_sizes_to_test,
    chromosome_lengths,
    fasta_name = 'genome'
) {
  library(BSgenome)

  fasta_name <- paste0(fasta_name, '_gc.fasta')

  df_combined <- df_combined %>%
    dplyr::mutate(unique_id = seq(1, dplyr::n()))

  df_combined_expanded <- df_combined %>%
    dplyr::mutate(key = 1) %>%
    dplyr::full_join(data.frame(window_size = window_sizes_to_test, key = 1), by = 'key') %>%
    dplyr::select(-key)

  df_combined_expanded <- merge(
    df_combined_expanded,
    chromosome_lengths,
    by = 'seqname'
  )

  df_combined_expanded <- df_combined_expanded %>%
    dplyr::mutate(
      start = start - window_size/2,
      end = end + window_size/2,
      start = ifelse(start < 1, 1, start),
      end = ifelse(end > chr_length, chr_length, end)
    ) %>%
    dplyr::select(-mid)

  df_combined_expanded_granges <- df_combined_expanded %>%
    GenomicRanges::makeGRangesFromDataFrame(keep.extra.columns = TRUE)

  alphabet_frequency <- Biostrings::getSeq(genomic_sequence, df_combined_expanded_granges) %>%
    Biostrings::alphabetFrequency() %>%
    as.data.frame()

  df_combined_result <- cbind(
    df_combined_expanded,
    alphabet_frequency
  )

  df_combined_result <- df_combined_result %>%
    dplyr::mutate(
      GC_content = (`G` + `C`) / (`G` + `C` + `A` + `T`),
      window_size = paste0(fasta_name, '.', window_size)
    ) %>%
    dplyr::select(unique_id, GC_content, window_size) %>%
    tidyr::pivot_wider(names_from = 'window_size', values_from = 'GC_content')

  df_combined <- merge(
    df_combined,
    df_combined_result,
    by = 'unique_id',
    all.x = TRUE
  ) %>%
    dplyr::select(-unique_id)

  return(df_combined)
  }
