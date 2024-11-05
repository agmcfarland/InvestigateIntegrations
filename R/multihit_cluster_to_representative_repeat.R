#' Assign Representative Repeat Class to Multihit Clusters
#'
#' This is a wrapper function that goes from a raw AAVengeR multihits table and corresponding repeat table to the most representative repeat class per multihit cluster.
#'
#'
#' @param aavenger_mhc A data frame representing the AAvengeR MHC data, with positional and cluster information.
#' @param repeat_table_grange A `GRanges` object containing repeat regions to check for overlaps.
#' @param repeat_column_name A character string indicating the column name that stores repeat classification information in the overlap results. Default is `"repeat_class_subject"`.
#' @return A data frame with the most abundant repeat class for each multihit cluster, containing:
#'   \describe{
#'     \item{trial}{Trial identifier.}
#'     \item{subject}{Subject identifier.}
#'     \item{sample}{Sample identifier.}
#'     \item{clusterID}{Cluster identifier.}
#'     \item{repeat_class}{The most abundant repeat class in the cluster.}
#'     \item{repeat_class_count}{Count of the most abundant repeat class.}
#'     \item{total_repeat_detections}{Total repeat detections in the cluster.}
#'     \item{percentage_of_repeat_detections}{Percentage of the most abundant repeat class in the cluster.}
#'   }
#' @details The function performs the following steps:
#'   \enumerate{
#'     \item Finds overlaps between `aavenger_mhc` and `repeat_table_grange` for multihit clusters using `find_overlaps_in_repeats()`.
#'     \item Counts occurrences of each repeat class within each cluster using `count_repeat_class_per_cluster()`.
#'     \item Extracts the most abundant repeat class for each cluster with `extract_most_abundant_repeat_class_per_cluster()`.
#'   }
#' @import GenomicRanges
#' @export
multihit_cluster_to_representative_repeat <- function(aavenger_mhc, repeat_table_grange, repeat_column_name = 'repeat_class_subject') {

  overlap_with_repeat_results <- find_overlaps_in_repeats(aavenger_table = aavenger_mhc, repeat_table_grange = repeat_table_grange, site_type = 'multihits')

  count_results <- count_repeat_class_per_cluster(aavenger_mhc_repeat_overlaps = overlap_with_repeat_results, repeat_column_name = repeat_column_name)

  mhc_repeats_counts_per_cluster <- extract_most_abundant_repeat_class_per_cluster(count_results)

  return(mhc_repeats_counts_per_cluster)
}
