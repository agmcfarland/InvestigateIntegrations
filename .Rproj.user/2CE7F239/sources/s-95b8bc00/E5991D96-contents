#' Fasta to Chromosome Lengths
#'
#' @description Converts a fasta file into a dataframe with two columns: chromosome name and chromosome length.
#'
#' @param genome_fasta  A `ShortRead` object with `@id` and `@width` values.
#'
#' @return A `data.frame` object with two columns: `seqname` and `chr_length`.
#'
#' @examples
#' fasta_to_chromosome_lengths(genome_fasta)
#'
#' @details This function explicitly shows how a genomic fasta is converted into the chromosome_lengths input used in other functions. A genomic fasta
#' file read in with `ShortRead::readFasta()` will generate the required inputs for this function.
#'
#' @import ShortRead dplyr
#'
#' @importFrom Biostrings DNAStringSet
#' @importFrom Biostrings BStringSet
#'
#' @export
fasta_to_chromosome_lengths <- function(genome_fasta){
  chromosome_lengths <- data.frame(
    'seqname' = ShortRead::id(genome_fasta),
    'chr_length' = ShortRead::width(genome_fasta)
  )%>%
    dplyr::arrange(desc(chr_length))

  return(chromosome_lengths)
}
