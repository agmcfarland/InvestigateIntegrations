#' hotROCs Compare Insertion to Match
#'
#' @description Supplies a random-matched set of integration sites to `hotROCs::ROC.ORC()`.
#'
#' @param matched_overlap_df A `data.frame` object containing specifically formatted match and insertion sites for each heatmap_group. See output of `test_for_overlaps()`.

#' @return Outputs the multidata object produced by `hotROCs::ROC.ORC()`.
#'
#' @examples
#' hotroc_compare_insertion_to_match(matched_overlap_df)
#'
#' @details This function wraps `hotROCs::ROC.ORC()`. It is meant to abstract away the input format details for this function as they can be confusing. The input for this function will have the same columns as the output of
#' `test_for_overlaps()` but it must include both sites of `type` 'match' and 'insertion' for each unique `heatmap_group`.
#'
#' @import hotROCs
#'
#' @export
hotroc_compare_insertion_to_match <- function(matched_overlap_df){
  hot_roc_result <- hotROCs::ROC.ORC(
    response = matched_overlap_df$type,
    variables = matched_overlap_df %>%
      dplyr::select(-c(seqname, start, end, width, strand, mid, type, heatmap_group)), # variables are all columns that contain genomic overlap counts for each genomic window of a genomic feature.
    origin = matched_overlap_df$heatmap_group)

  return(hot_roc_result)
}
