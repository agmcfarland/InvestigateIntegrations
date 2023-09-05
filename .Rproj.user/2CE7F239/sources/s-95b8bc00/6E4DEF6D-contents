#' Sample Genome
#'
#' @description Randomly sample positions in a genome.
#'
#' @param number_of_random_positions  A `numeric` value that specifies how many different random positions will be generated.
#' @param chromosome_lengths A `data.frame` object. See `fasta_to_chromosome_lengths()`.
#' @param random_seed_value A `numeric` value to use as a seed for the random number generator function `r.seed()`.
#'
#' @return A `data.frame` object with two columns: `seqname` and `random_positions`.
#'
#' @examples
#' sample_genome(number_of_random_positions, chromosome_lengths, random_seed_value)
#'
#' @details Sampling is performed with replacement. The `data.frame` `chromosome_lengths` object is used so that users know what is being sampled is a dataframe of chromosomal lengths.
#'
#' @import dplyr
#'
#' @export
sample_genome <- function(
    number_of_random_positions,
    chromosome_lengths,
    random_seed_value
){

  # Determine the number of times each chromosome will be sampled randomly. The number of times any chromosome will be sampled is a function of its chromosomal length.
  set.seed(random_seed_value)
  random_chromosome_samples <- sample(
    x = chromosome_lengths$seqname,
    size = number_of_random_positions,
    prob = chromosome_lengths$chr_length,
    replace=TRUE)

  # Convert list of chromosome names into a dataframe with one row per chromosome id and the corresponding amount it is to be sampled
  # This dataframe will be the main object used for sampling downstream
  random_chromosome_samples <- data.frame(
    'seqname' = random_chromosome_samples,
    'count' = 1)%>%
    dplyr::group_by(seqname)%>%
    dplyr::mutate(sample_n = sum(count))%>%
    dplyr::ungroup()%>%
    dplyr::select(-count)%>%
    base::unique()%>%
    dplyr::arrange(desc(sample_n))

  # Add the lengths of each chromosome to the dataframe
  random_chromosome_samples <- merge(
    random_chromosome_samples,
    chromosome_lengths,
    by = 'seqname')

  # Randomly sample each chromsome sample_n amount of times using chr_length as the upper limit
  # The output will be a list of named integers. Each integer is named by the chromosome it originated from
  random_chromosome_positions <- lapply(1:nrow(random_chromosome_samples), function(x){
    sample_ouput <- sample(
      random_chromosome_samples[x, 'chr_length'],
      random_chromosome_samples[x, 'sample_n'],
      replace = FALSE
    )
    names(sample_ouput) <- rep(random_chromosome_samples[x, 'seqname'], length(sample_ouput))
    return(sample_ouput)
  }
  )

  # Convert the list to a dataframe consisting of chromosome names and random positions
  final_random_chromosome_positions <- data.frame(
    'seqname' = names(unlist(random_chromosome_positions)),
    'random_positions' = unlist(random_chromosome_positions)
  )

  return(final_random_chromosome_positions)
}
