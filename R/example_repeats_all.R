#' Example Genomic Ranges with Repeat Annotations
#'
#' A `GRanges` object representing genomic ranges with associated repeat class annotations, used for analyses involving repetitive elements across the genome.
#'
#' @format A `GRanges` object with 6 ranges and 1 metadata column:
#' \describe{
#'   \item{seqnames}{Rle, chromosome or sequence names where each range is located (e.g., chr1, chr10).}
#'   \item{ranges}{IRanges, start and end positions of each range within the chromosome.}
#'   \item{strand}{Rle, strand information (`*` indicates no strand specificity).}
#'   \item{repeat_class}{Character, metadata column indicating the repeat class of each range, such as "SINE," "LINE," or "simple_repeat".}
#' }
#'
#' @details This dataset includes genomic ranges with annotated repeat classes, useful for analyzing repetitive elements. The `seqinfo` metadata contains 25 sequences from an unspecified genome with undefined sequence lengths.
#'
#' @source Generated example data for genomic analysis.
'example_repeats_all'
