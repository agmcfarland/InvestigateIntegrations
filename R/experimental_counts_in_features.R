#' Calculate experimental integration counts in genomic features for multiple scenarios
#'
#' This function calculates the number of experimental genomic integrations within specified genomic features for multiple scenarios defined by varying parameters.
#'
#' @param df_sites A `data.frame` containing experimental genomic site information, including columns `seqname`, `start`, `end`, `width`, `strand`, `mid`, `type`, `heatmap_group`.
#'#'
#' @param overlap_window_sizes A numeric vector containing different sizes of overlap windows around each integration site.
#'
#' @param feature_files_to_process A character vector containing absolute paths to feature files for processing. Must be `GenomicRanges` objects in .rds or RData (named epigenData if RData) format. See `example_refGene_rds`
#'
#' @param number_of_cores The number of CPU cores to use for parallel processing. Defaults to the number of available cores.
#'
#' @return A data frame containing experimental integration counts and percentages in features, including overlap window size, feature track file, heatmap group,
#' count of integrations in features, total integration sites, and percentage of integrations in features.
#'
#' @examples
#' # Example usage:
#' df_sites <- format_aavenger_sites(unformatted_aavenger_sites)
#' overlap_window_sizes <- c(10, 20, 30)
#' feature_files <- c("path/to/feature_file1.rds", "path/to/feature_file2.rds")
#' experimental_counts_in_features(df_sites, chromosome_lengths, overlap_window_sizes, feature_files)
#'
#' @import dplyr
#' @import parallel
#'
#' @export
#'
experimental_counts_in_features <- function(df_sites, chromosome_lengths,  overlap_window_sizes, feature_files_to_process, number_of_cores = parallel::detectCores()) {

  process_counts_experimental <- function(heatmap_group_) {
    df_sites_filtered <- df_sites %>%
      dplyr::filter(heatmap_group == heatmap_group_)

    overlap_count_result <- count_integrations_in_feature_window(
      df_sites = df_sites_filtered,
      feature_file = feature_track_file,
      overlap_window_size = single_overlap_value
    )

    result <- data.frame(
      'overlap_window_size' = single_overlap_value,
      'feature_track' = feature_track_file,
      'heatmap_group' = heatmap_group_,
      'count_in_feature' = overlap_count_result[1],
      'total_sites' = overlap_count_result[2],
      'percentage_integrations_in_feature' = overlap_count_result[3]
    )

    return(result)
  }

  # store results here
  store_experimental_results <- data.frame()

  cl <- parallel::makeCluster(number_of_cores)

  for (feature_track_file in feature_files_to_process){

    print(feature_track_file)

    for (single_overlap_value in overlap_window_sizes){

      # Export necessary variables to the cluster workers
      parallel::clusterExport(cl, c('df_sites', 'feature_track_file', 'single_overlap_value'), envir = environment())

      # Parallelize the loop using mclapply
      parallel_experimental_results <-  parallel::mclapply(base::unique(df_sites$heatmap_group), process_counts_experimental, mc.cores = number_of_cores)

      # Aggregate into a single dataframe
      parallel_experimental_results <- do.call(rbind, parallel_experimental_results)

      # Combine the results into a data frame
      store_experimental_results <- rbind(store_experimental_results, parallel_experimental_results)
    }
  }
  parallel::stopCluster(cl)

  return(store_experimental_results)

}
