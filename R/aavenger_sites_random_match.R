#' AAvengeR Sites Random Match
#'
#' @description Create a random-matched dataframe for each `heatmap_group` combination in `aavenger_sites`
#'
#' @param matched_aavenger_sites_df A `data.frame` object containing specifically formatted match and insertion sites for each heatmap_group. See `format_aavenger_sites()`.
#' @param chromosome_lengths A `data.frame` object. See `fasta_to_chromosome_lengths()`.
#' @param random_seed_value A `numeric` value to use as a seed for the random number generator function `r.seed()`.
#' @param match_row_number_modifier A `integer` value that will be used to multiply the number of rows in the insertion `heatmap_group` to obtain the number of sites to sample for the random match.

#' @return A `data.frame` object of random-matches for each unique `heatmap_group` in the supplied `aavenger_sites`. The columns will be match the output of `format_aavenger_sites()`.
#'
#' @examples
#' aavenger_sites_random_match(aavenger_sites, chromosome_lengths, random_seed_value)
#'
#' @details This function acts as a wrapper for `sample_genome()`.
#'
#' @import dplyr
#'
#' @export
aavenger_sites_random_match <- function(
    aavenger_sites,
    chromosome_lengths,
    random_seed_value,
    match_row_number_modifier = 3){

  # Get list of unique heatmap groups
  unique_heatmap_groups <- unique(aavenger_sites$heatmap_group)

  # For each heatmap group, randomly sample the genome X number of times to create a random-matched dataframe
  store_random_match_df <- lapply(unique_heatmap_groups, function(x){
    number_of_insertion_rows <- nrow(aavenger_sites %>%
                                       dplyr::filter(heatmap_group == x))
    number_of_random_match_rows_to_create <- number_of_insertion_rows * match_row_number_modifier

    random_match <- sample_genome(
      number_of_random_positions = number_of_random_match_rows_to_create,
      chromosome_lengths = chromosome_lengths,
      random_seed_value = random_seed_value
    )

    random_match <- random_match%>%
      dplyr::mutate(
        start = random_positions,
        end = random_positions,
        width = 1,
        strand = '*',
        mid = random_positions,
        type = 'match',
        heatmap_group = x)%>%
      dplyr::select(seqname, start, end, width, strand, mid, type, heatmap_group)


    return(random_match)
  })

  store_random_match_df <- do.call(rbind, store_random_match_df)

  return(store_random_match_df)

}
