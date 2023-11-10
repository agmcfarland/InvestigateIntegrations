#' Simulate integrations in genomic features for multiple scenarios
#'
#' Gets count and percentages of bootstrapped simulated integration sites in feature and overlap window combinations.
#'
#' @param number_of_simulations The number of bootstrap simulations of integration sites distributions.
#'
#' @param number_of_random_positions The number of random integration sites to generate in each simulation.
#'
#' @param chromosome_lengths A `data.frame` object with chromosome and length columns. See `fasta_to_chromosome_lengths()`.
#'
#' @param overlap_window_sizes A numeric vector containing different sizes of overlap windows around each integration site.
#'
#' @param feature_files_to_process A character vector containing absolute paths to feature files for processing. Must be `GenomicRanges` objects in .rds or RData (named epigenData if RData) format. See `example_refGene_rds`
#'
#' @param base_random_seed_value The base random seed value used for generating random integration sites. Each simulation will use a different seed based off this seed.
#'
#' @param number_of_cores The number of CPU cores to use for parallel processing. Defaults to the number of available cores.
#'
#' @return A data frame containing simulation results, including overlap window size, feature track file, simulation number,
#' count of integrations in features, total integration sites, and percentage of integrations in features.
#'
#' @examples
#' # Example usage:
#' number_of_simulations <- 10
#' number_of_random_positions <- 1000
#' chromosome_lengths <- c("chr1" = 500, "chr2" = 700)
#' overlap_window_sizes <- c(10, 20, 30)
#' feature_files <- c("path/to/feature_file1.rds", "path/to/feature_file2.rds")
#' base_random_seed_value <- 123
#' simulate_many_counts_in_features(number_of_simulations, number_of_random_positions, chromosome_lengths, overlap_window_sizes, feature_files, base_random_seed_value)
#'
#' @import dplyr
#' @import parallel
#'
#' @export
#'
simulate_many_counts_in_features <- function(number_of_simulations, number_of_random_positions,  chromosome_lengths,  overlap_window_sizes, feature_files_to_process,
                                             base_random_seed_value, number_of_cores = parallel::detectCores()) {

  process_counts_simulation <- function(sim_n) {
    # Function within a function to protect against accidental environment scope problems.
    # Counts number of overlaps and returns a formatted dataframe row.

    df_random_sites <- combined_dataframes %>%
      dplyr::filter(simulation_id == sim_n)

    overlap_count_result <- count_integrations_in_feature_window(
      df_sites = df_random_sites,
      feature_file = feature_track_file,
      overlap_window_size = single_overlap_value
    )

    result <- data.frame(
      'overlap_window_size' = single_overlap_value,
      'feature_track' = feature_track_file,
      'sim_n' = sim_n,
      'count_in_feature' = overlap_count_result[1],
      'total_sites' = number_of_random_positions,
      'percentage_integrations_in_feature' = overlap_count_result[3]
    )

    return(result)
  }

  # Bootstrap integration site distribution and combine into a single dataframe
  combined_dataframes <- data.frame()
  for ( sim_n in 1:number_of_simulations) {
    df_random_sites <- sample_genome(
      number_of_random_positions = number_of_random_positions,
      chromosome_lengths = chromosome_lengths,
      random_seed_value = base_random_seed_value * sim_n
    ) %>%
      dplyr::mutate(
        simulation_id = sim_n)#,
    # random_seed_value = base_random_seed_value * sim_n)
    combined_dataframes <- rbind(combined_dataframes, df_random_sites)
  }

  # store results testing simulated integrations in feature and overlap window here
  store_simulation_results <- data.frame()

  cl <- parallel::makeCluster(number_of_cores)

  for (feature_track_file in feature_files_to_process){

    print(feature_track_file)

    for (single_overlap_value in overlap_window_sizes){

      # Export necessary variables to the cluster workers
      parallel::clusterExport(cl, c('combined_dataframes', 'feature_track_file', 'single_overlap_value', 'number_of_random_positions'),
                              envir = environment())

      # Parallelize the loop using mclapply
      parallel_simulation_results <-  parallel::mclapply(1:number_of_simulations, process_counts_simulation, mc.cores = number_of_cores)

      # Aggregate into a single dataframe
      parallel_simulation_results <- do.call(rbind, parallel_simulation_results)

      # Combine the results into a data frame
      store_simulation_results <- rbind(store_simulation_results, parallel_simulation_results)
    }
  }
  # Stop the cluster
  parallel::stopCluster(cl)

  return(store_simulation_results)

}
