#' Format AAVengeR sites
#'
#' @description Formats an AAvengeR sites `dataframe` object so that it is usable in all downstream functions.
#'
#' @param aavenger_sites_df  A `data.frame` object. Must at least contain the following columns:
#' \describe{
#' \item{posid}{The integration site position in the AAVengeR format.}.
#' \item{trial}{The trial name.}
#' \item{subject}{The subject name.}
#' \item{sample}{The sample name.}}
#'
#' @return A `data.frame` object with the following columns: `seqname`, `start`, `end`, `width`, `strand`, `mid`, `type`, `heatmap_group`
#'
#' @examples
#' format_aavenger_sites(aavenger_sites_df)
#'
#' @details This function will paste together the trial, subject, and sample values into a single heatmap_group value. It will also assign
#' `type` to be `insertion`.
#'
#' @import dplyr
#'
#' @export
format_aavenger_sites <- function(aavenger_sites_df){

  aavenger_sites_df <- aavenger_sites_df%>%
    tidyr::separate(
      col = posid,
      into = c('chromosome', 'position', 'extra'),
      remove = TRUE
    )%>%
    dplyr::select(chromosome, position, trial, subject, sample)%>%
    dplyr::rename(seqname = chromosome)%>%
    dplyr::mutate(
      start = position,
      end = position,
      width = 1,
      strand = '*',
      mid = position,
      type = 'insertion',
      heatmap_group = paste(trial, subject, sample, sep = '_')
    )%>%
    dplyr::select(-c(position, trial, subject, sample))%>%
    dplyr::mutate(
      start = as.integer(start),
      end = as.integer(end),
      mid = as.integer(mid)
    )

  return(aavenger_sites_df)
}
