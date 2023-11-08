#' Test for Overlaps
#'
#' @description Count the number of times an integration site overlaps with a genomic feature.
#'
#' @param matched_aavenger_sites_df A `data.frame` object containing specifically formatted match and insertion sites for each heatmap_group. See `format_aavenger_sites()`.
#' @param list_of_feature_files A `list` object of `rds` or `RData` files containing a `GRanges` object called `epigenData`. The file path must be a prefix and the file type must be the suffix.
#' The data required in the `GRanges` object is listed below:
#' \describe{
#' \item{seqname}{name of the chromosome. Must match those in `matched_aavenger_sites_df`}.
#' \item{ranges}{An `IRanges` object with a genomic range.}
#' \item{Rle}{The strand the range is located in. Should be set to `*`.}}
#' @param overlap_ranges_to_test A `numeric` `vector` of the different genomic windows to use in the overlap test. Values must be integers.
#'
#' @return A dataframe containing the original `matched_aavenger_sites_df` and a column of overlap counts per each file from `list_of_feature_files`.
#'
#' @examples
#' test_for_overlaps(matched_aavenger_sites_df, list_of_feature_files = list_of_feature_filenames,data_dir=file.path('/path/to/feature_filenames'),overlap_ranges_to_test = c(1000,10000,1000000))
#'
#' @import GenomicRanges tibble stringr dplyr
#'
#' @export
test_for_overlaps <- function(
    matched_aavenger_sites_df,
    list_of_feature_files,
    overlap_ranges_to_test
){

  if (length(list_of_feature_files) == 0) {
    stop('argument "list_of_feature_files" is empty.')
  }

  # Make a string version of overlap ranges to test
  options(scipen = 999) # ensures that 1000000 doesn't turn into 1e6, etc
  overlap_ranges_to_test_string <- as.character(overlap_ranges_to_test)

  ## Make empty dataframe to store results of test
  # names of columns in vector format
  store_overlap_results_colnames <- outer(basename(list_of_feature_files), overlap_ranges_to_test_string, paste, sep = '.') # take the basename() of list_of_feature_files to remove file paths
  store_overlap_results_colnames <- as.vector(t(store_overlap_results_colnames))

  # Create an empty matrix of the following dimensions
  # number of rows = number of all sites to test
  # number of columns = number of different overlaps to test multipled by the number of different datasets to test
  store_overlap_results <- matrix(
    0, #fill with zeros
    nrow = nrow(matched_aavenger_sites_df),
    ncol = length(store_overlap_results_colnames),
    dimnames = list(NULL,store_overlap_results_colnames)
  )

  store_overlap_results <- tibble::as_tibble(store_overlap_results) # why is this as_tibble instead of a dataframe?

  ## Count overlaps for each overlap range in each dataset
  # Make a genomic ranges version of the submit dataframe
  matched_aavenger_sites_df_genomic_ranges <- GenomicRanges::makeGRangesFromDataFrame(matched_aavenger_sites_df, keep.extra.columns = TRUE)

  # Begin for loop to loop over biological RData objects
  for (biological_data in list_of_feature_files){
    print(biological_data)

    # check whether rds file else assume RData / .rda. Either way, variable with GRanges is called epigenData
    if (stringr::str_ends(biological_data, '.rds')){
      epigenData <- readRDS(biological_data)
    } else{
      load(file.path(biological_data))
    }

    # Enter loop. For each single overlap range value count the genomic overlaps
    for (single_overlap_value in overlap_ranges_to_test){

      # name of the biological dataset and the overlap value to be used. will match a column name in the pre-created `store_overlap_results`
      data_name_and_overlap_value <- paste(basename(biological_data), single_overlap_value, sep = '.') # take the basename() of biological_data to remove file path

      # count the overlaps
      overlap_count_results <- GenomicRanges::countOverlaps(
        query = matched_aavenger_sites_df_genomic_ranges, # The GRanges object containing ALL sites to be tested for overlaps
        subject = epigenData, # epigenData is name of the GRanges object that is loaded in the biological .RData file
        maxgap = single_overlap_value/2, # the overlap value is halved and subtracted from the single value range object to create a range
        ignore.strand = TRUE
      )

      # store the overlaps in the corresponding column
      store_overlap_results[data_name_and_overlap_value] <- overlap_count_results
    }
  }

  ## Create a genomic ranges object
  # Start with the dataframe of insertion sites `matched_aavenger_sites_df` and the overlap test results `store_overlap_results`
  combined_overlap_test_results_genomic_ranges <- cbind(
    matched_aavenger_sites_df,
    store_overlap_results
  )
}
