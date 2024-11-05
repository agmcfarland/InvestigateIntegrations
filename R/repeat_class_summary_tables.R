#' Generate Summary Tables of Repeat Class Counts and Percentages
#'
#' This function creates summary tables for repeat class counts and percentages, based on unique and multihit clusters, and combines them into a single table.
#'
#' @param aavenger_unique_sites_table A data frame containing unique site information with columns including `trial`, `subject`, `sample`, `posid`, and `repeat_class`.
#' @param mhc_repeats_counts_per_cluster A data frame containing counts of repeat classes for each multihit cluster, including columns such as `trial`, `subject`, `sample`, `clusterID`, `repeat_class`, and `repeat_class_count`.
#' @return A named list of three data frames:
#'   \describe{
#'     \item{unique}{Summary table for unique repeat detections, with columns `trial`, `subject`, `sample`, `repeat_class`, `repeat_class_count`, `total_repeat_detections`, and `percentage_of_repeat_detections`.}
#'     \item{multihits}{Summary table for multihit repeat detections, with similar columns as the unique table.}
#'     \item{combined}{Combined summary table, consolidating unique and multihit repeat counts and percentages by repeat class for each sample.}
#'   }
#' @details The function performs the following steps:
#'   \enumerate{
#'     \item Counts and calculates percentages for repeat classes in the `aavenger_unique_sites_table` (unique sites).
#'     \item Counts and calculates percentages for repeat classes in the `mhc_repeats_counts_per_cluster` (multihit clusters).
#'     \item Combines unique and multihit tables, recalculating the counts and percentages of repeat classes.
#'   }
#' @import dplyr
#' @export
repeat_class_summary_tables <- function(aavenger_unique_sites_table, mhc_repeats_counts_per_cluster) {

  testthat::expect_equal('repeat_class' %in% colnames(aavenger_unique_sites_table), TRUE)

  testthat::expect_equal('repeat_class' %in% colnames(mhc_repeats_counts_per_cluster), TRUE)

  df_unique_repeat_counts <- aavenger_unique_sites_table %>%
    # function start here
    dplyr::select(trial, subject, sample, posid, repeat_class) %>%
    dplyr::group_by(trial, subject, sample, posid) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(trial, subject, sample) %>%
    dplyr::mutate(total_repeat_detections = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(trial, subject, sample, repeat_class) %>%
    dplyr::mutate(
      repeat_class_count = dplyr::n(),
      percentage_of_repeat_detections = 100 * (repeat_class_count/ total_repeat_detections)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(trial, subject, sample, repeat_class, repeat_class_count, total_repeat_detections, percentage_of_repeat_detections) %>%
    base::unique()


  # Multihit repeats
  df_multihit_repeat_counts <- mhc_repeats_counts_per_cluster %>%
    dplyr::group_by(trial, subject, sample) %>%
    dplyr::mutate(total_repeat_detections = dplyr::n()) %>%
    dplyr::group_by(trial, subject, sample, repeat_class) %>%
    dplyr::mutate(
      repeat_class_count = dplyr::n(),
      percentage_of_repeat_detections = 100 * (repeat_class_count/ total_repeat_detections)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(trial, subject, sample, repeat_class, repeat_class_count, total_repeat_detections, percentage_of_repeat_detections) %>%
    base::unique()


  # Combined repeats
  df_combined <- rbind(
    df_unique_repeat_counts,
    df_multihit_repeat_counts
  )

  df_combined <- df_combined %>%
    dplyr::group_by(trial, subject, sample) %>%
    dplyr::mutate(total_repeat_detections = sum(repeat_class_count)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(trial, subject, sample, repeat_class) %>%
    dplyr::mutate(
      repeat_class_count = sum(repeat_class_count),
      percentage_of_repeat_detections = 100 * (repeat_class_count/ total_repeat_detections)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(trial, subject, sample, repeat_class, repeat_class_count, total_repeat_detections, percentage_of_repeat_detections) %>%
    base::unique()

  output_tables <- list(df_unique_repeat_counts, df_multihit_repeat_counts, df_combined)

  names(output_tables) <- c('unique', 'multihits', 'combined')

  return(output_tables)
}
