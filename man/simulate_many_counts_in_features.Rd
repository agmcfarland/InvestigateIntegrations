% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_many_counts_in_features.R
\name{simulate_many_counts_in_features}
\alias{simulate_many_counts_in_features}
\title{Simulate integrations in genomic features for multiple scenarios}
\usage{
simulate_many_counts_in_features(
  number_of_simulations,
  number_of_random_positions,
  chromosome_lengths,
  overlap_window_sizes,
  feature_files_to_process,
  base_random_seed_value,
  number_of_cores = parallel::detectCores()
)
}
\arguments{
\item{number_of_simulations}{The number of bootstrap simulations of integration sites distributions.}

\item{number_of_random_positions}{The number of random integration sites to generate in each simulation.}

\item{chromosome_lengths}{A \code{data.frame} object with chromosome and length columns. See \code{fasta_to_chromosome_lengths()}.}

\item{overlap_window_sizes}{A numeric vector containing different sizes of overlap windows around each integration site.}

\item{feature_files_to_process}{A character vector containing absolute paths to feature files for processing. Must be \code{GenomicRanges} objects in .rds or RData (named epigenData if RData) format. See \code{example_refGene_rds}}

\item{base_random_seed_value}{The base random seed value used for generating random integration sites. Each simulation will use a different seed based off this seed.}

\item{number_of_cores}{The number of CPU cores to use for parallel processing. Defaults to the number of available cores.}
}
\value{
A data frame containing simulation results, including overlap window size, feature track file, simulation number,
count of integrations in features, total integration sites, and percentage of integrations in features.
}
\description{
Gets count and percentages of bootstrapped simulated integration sites in feature and overlap window combinations.
}
\examples{
# Example usage:
number_of_simulations <- 10
number_of_random_positions <- 1000
chromosome_lengths <- c("chr1" = 500, "chr2" = 700)
overlap_window_sizes <- c(10, 20, 30)
feature_files <- c("path/to/feature_file1.rds", "path/to/feature_file2.rds")
base_random_seed_value <- 123
simulate_many_counts_in_features(number_of_simulations, number_of_random_positions, chromosome_lengths, overlap_window_sizes, feature_files, base_random_seed_value)

}
