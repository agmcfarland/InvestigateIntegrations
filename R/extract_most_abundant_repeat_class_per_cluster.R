#' Extract Most Abundant Repeat Class Per Cluster
#'
#' This function identifies the most abundant repeat class within each cluster based on the highest percentage of repeat detections.
#'
#' @param mhc_repeats_counts_per_cluster A data frame containing counts and percentages of repeat classes per cluster, with columns such as `trial`, `subject`, `sample`, `clusterID`, and `percentage_of_repeat_detections`.
#' @return A data frame with the most abundant repeat class per cluster, containing:
#'   \describe{
#'     \item{trial}{Trial identifier.}
#'     \item{subject}{Subject identifier.}
#'     \item{sample}{Sample identifier.}
#'     \item{clusterID}{Cluster identifier.}
#'     \item{repeat_class}{The most abundant repeat class within the cluster.}
#'     \item{repeat_class_count}{Count of the most abundant repeat class.}
#'     \item{total_repeat_detections}{Total repeat detections in the cluster.}
#'     \item{percentage_of_repeat_detections}{Percentage of the most abundant repeat class in the cluster.}
#'   }
#' @details Extracts the most abundant repeat from a multihit clusters object. When there are ties picks the first repeat in the dataframe.
#' @import dplyr
#' @export
extract_most_abundant_repeat_class_per_cluster <- function(mhc_repeats_counts_per_cluster) {
  mhc_repeats_counts_per_cluster <- mhc_repeats_counts_per_cluster %>%
    dplyr::group_by(trial, subject, sample, clusterID) %>%
    dplyr::arrange(dplyr::desc(percentage_of_repeat_detections)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  return(mhc_repeats_counts_per_cluster)
}
