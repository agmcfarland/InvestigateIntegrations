#' Find Overlaps in Repeat Regions
#'
#' This function identifies overlaps between positions in an AAvengeR table and repeat regions. It returns a table of overlaps, marking both the query and subject ranges.
#'
#' @param aavenger_table A data frame containing AAvengeR data, including positional identifiers that can be split into chromosome and position information.
#' @param repeat_table_grange A `GRanges` object containing repeat regions to compare against.
#' @param site_type A character string specifying the site type, either `"unique"` or `"multihits"`. Must be one of these two values.
#' @return A data frame or `GRanges` object with the overlaps between `aavenger_table` and `repeat_table_grange`. The result includes columns marking the overlap for both the query (`aavenger_table`) and subject (`repeat_table_grange`) ranges.
#' @details Can take as input either an aavenger unique sites table or an aavenger multihits cluster table.
#'   \itemize{
#'     \item For `"unique"`: positions are extracted, converted to genomic ranges with `start` and `end` set to `position`.
#'     \item For `"multihits"`: the table is unnested, split into `chromosome` and `position`, and converted to genomic ranges.
#'   }
#'   The function then calls `extract_overlaps()` to identify overlaps between the processed `aavenger_table` and `repeat_table_grange`.
#' @import testthat
#' @import dplyr
#' @import GenomicRanges
#' @export
find_overlaps_in_repeats <- function(aavenger_table, repeat_table_grange, site_type) {

  testthat::expect_true(site_type %in% c('unique', 'multihits'), info = 'site type must be "unique" or "multihits"')

  if (site_type == 'unique') {
    aavenger_granges <- aavenger_table %>%
      split_posid_into_chromosome_position () %>%
      dplyr::mutate(
        start = position,
        end = position) %>%
      GenomicRanges::makeGRangesFromDataFrame(keep.extra.columns = TRUE)
  }

  if (site_type == 'multihits') {
    aavenger_granges <- aavenger_table %>%
      unnest_mhc_table() %>%
      split_posid_into_chromosome_position() %>%
      dplyr::rename(start = position) %>%
      dplyr::mutate(end = start) %>%
      GenomicRanges::makeGRangesFromDataFrame(keep.extra.columns = TRUE)
    }

  return(
    extract_overlaps(
      aavenger_granges,
      repeat_table_grange,
      mark_query = TRUE,
      mark_subject = TRUE,
      keep_all_query = TRUE
      )
    )
}
