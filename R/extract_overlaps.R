#' Extract overlapping regions between two sets of genomic ranges
#'
#' This function identifies overlapping regions between a query set and a subject set of genomic ranges.
#'
#' @param query A GenomicRanges object representing the query set. Must have start, end, strand columns.
#' @param subject A GenomicRanges object representing the subject set. Must have start, end, strand columns.
#' @param mark_query Logical indicating whether to mark columns of the query set with "_query" suffix.
#' @param mark_subject Logical indicating whether to mark columns of the subject set with "_subject" suffix.
#' @details Output is a data table with all instances of query in subject, even if query has multiple hits in subject.
#'  All columns from query and subject are joined in the output via cbind.
#' @return A data frame containing all overlapping regions between the query and subject sets.
#' @import dplyr GenomicRanges
#' @export
extract_overlaps <- function(query, subject, mark_query = TRUE, mark_subject = TRUE) {

  find_overlap_results <- GenomicRanges::findOverlaps(
    query = query,
    subject = subject,
    select = 'all',
    ignore.strand = TRUE
  )

  query <- query[find_overlap_results@from] %>%
    as.data.frame()

  subject <- subject[find_overlap_results@to] %>%
    as.data.frame()

  if (mark_query) {
    colnames(query) <- paste0( colnames(query), '_query')
  }

  if (mark_subject) {
    colnames(subject) <- paste0( colnames(subject), '_subject')
  }

  return(cbind(query, subject))
}
