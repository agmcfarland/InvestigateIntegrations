#' Extract Overlapping Regions Between Genomic Ranges Sets
#'
#' Identifies and extracts overlapping regions between a query and a subject set of genomic ranges.
#' Optionally, non-overlapping regions from the query set can be included with missing values for subject columns.
#'
#' @param query A `GenomicRanges` object representing the query set, containing `start`, `end`, and `strand` columns.
#' @param subject A `GenomicRanges` object representing the subject set, containing `start`, `end`, and `strand` columns.
#' @param mark_query Logical, if `TRUE`, appends "_query" to column names in the query set within the output.
#' @param mark_subject Logical, if `TRUE`, appends "_subject" to column names in the subject set within the output.
#' @param keep_all_query Logical, if `TRUE`, includes all rows from the query set in the output, even if they have no match in the subject set, filling subject columns with `NA`.
#'
#' @details The function returns all instances of overlaps between `query` and `subject`, including cases where a single query region matches multiple subject regions. Columns from `query` and `subject` are combined in the output using `cbind`.
#' If `keep_all_query` is `TRUE`, non-overlapping query regions are included with `NA` in subject columns.
#'
#' @return A data frame containing all overlapping regions between the query and subject sets, with additional columns marking the source (query or subject) if `mark_query` or `mark_subject` is `TRUE`.
#'
#' @import dplyr GenomicRanges
#' @export
extract_overlaps <- function(query, subject, mark_query = TRUE, mark_subject = TRUE, keep_all_query = FALSE) {

  find_overlap_results <- GenomicRanges::findOverlaps(
    query = query,
    subject = subject,
    select = 'all',
    ignore.strand = TRUE
  )

  query_result <- query[find_overlap_results@from] %>%
    as.data.frame()

  subject_result <- subject[find_overlap_results@to] %>%
    as.data.frame()

  if (mark_query) {
    colnames(query_result) <- paste0( colnames(query_result), '_query')
  }

  if (mark_subject) {
    colnames(subject_result) <- paste0( colnames(subject_result), '_subject')
  }

  result <- cbind(query_result, subject_result)

  if (keep_all_query) {
    missing_query <- setdiff(seq(1, length(query)), find_overlap_results@from)

    query_nohit <- query[missing_query] %>%
      as.data.frame()

    if (mark_query) {
      colnames(query_nohit) <- paste0( colnames(query_nohit), '_query')
    }

    subject_nohit <- data.frame(matrix(NA, nrow = nrow(query_nohit), ncol = ncol(subject_result)))

    colnames(subject_nohit) <- colnames(subject_result)

    result_nohit <- cbind(query_nohit, subject_nohit)

    result <- rbind(result, result_nohit)
  }

  return(result)

}

