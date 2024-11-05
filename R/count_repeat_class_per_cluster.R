#' Count Repeat Classes Per Cluster
#'
#' This function calculates the count and percentage of different repeat classes within each cluster in the AAvengeR MHC data. It aggregates data by cluster and computes the proportion of each repeat class per cluster.
#'
#' @param aavenger_mhc_repeat_overlaps A data frame containing AAvengeR MHC repeat overlaps, with columns for repeat classifications and query data.
#' @param repeat_column_name A character string specifying the name of the column that contains repeat classification information. Default is `"repeat_class_subject"`.
#' @return A data frame with counts and percentages of each repeat class per cluster, including:
#'   \describe{
#'     \item{trial}{Trial identifier.}
#'     \item{subject}{Subject identifier.}
#'     \item{sample}{Sample identifier.}
#'     \item{clusterID}{Cluster identifier.}
#'     \item{repeat_class}{Simplified repeat class.}
#'     \item{repeat_class_count}{Count of each repeat class within the cluster.}
#'     \item{total_repeat_detections}{Total repeat detections in the cluster.}
#'     \item{percentage_of_repeat_detections}{Percentage of each repeat class in the cluster.}
#'   }
#' @details Counts the number of repeats found in a multihit clusters object outputted by AAVengeR.
#' @import testthat
#' @import dplyr
#' @import stringr
#' @export
count_repeat_class_per_cluster <- function(aavenger_mhc_repeat_overlaps, repeat_column_name = 'repeat_class_subject') {

  testthat::expect_equal(repeat_column_name %in% colnames(aavenger_mhc_repeat_overlaps), TRUE, info = paste0('Column "', repeat_column_name, '" not found'))

  aavenger_mhc_repeat_overlaps <- aavenger_mhc_repeat_overlaps %>%
    dplyr::select(dplyr::matches(paste0("_query|", repeat_column_name)))

  colnames(aavenger_mhc_repeat_overlaps) <- stringr::str_replace_all(colnames(aavenger_mhc_repeat_overlaps), '_query', '')

  aavenger_mhc_repeat_overlaps <- aavenger_mhc_repeat_overlaps %>%
    dplyr::rename_with(~ 'repeat_class', (repeat_column_name))

  df_mhc_repeats_counts <- aavenger_mhc_repeat_overlaps %>%
    relabel_repeat_categories() %>%
    dplyr::select(-repeat_class) %>%
    dplyr::rename(repeat_class = repeat_simplified) %>%
    dplyr::group_by(trial, subject, sample, clusterID, repeat_class) %>%
    dplyr::mutate(repeat_class_count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(trial, subject, sample, clusterID, repeat_class, repeat_class_count) %>%
    base::unique() %>%
    dplyr::group_by(trial, subject, sample, clusterID) %>%
    dplyr::mutate(
      total_repeat_detections = sum(repeat_class_count),
      percentage_of_repeat_detections = 100 * (repeat_class_count/total_repeat_detections)
    )

  return(df_mhc_repeats_counts)
}
