#' Split `posid` into Chromosome, Position, and Strand
#'
#' This function separates the `posid` column in a data frame into `chromosome`, `position`, and `extra` columns. It also extracts the strand information from `posid`.
#'
#' @param df A data frame containing a `posid` column, where each value is a string encoding chromosome, position, and optional strand information.
#' @return A data frame with new columns:
#'   \describe{
#'     \item{chromosome}{The chromosome component of `posid`.}
#'     \item{position}{The numeric position component of `posid`.}
#'     \item{extra}{A numeric extra component derived from `position`.}
#'     \item{strand}{The strand information, extracted as `+` or `-` from `posid`.}
#'   }
#' @details Splits the posid column from the AAVengeR output into `chromosome`, `position`, and `extra` columns. The `posid` column will remain.
#' @import tidyr
#' @import dplyr
#' @export

split_posid_into_chromosome_position <- function(df) {
  return(df %>%
           tidyr::separate(col = posid, into = c('chromosome', 'position', 'extra'), remove = FALSE) %>%
           dplyr::mutate(
             position = as.numeric(position),
             extra = as.numeric(position),
             strand = gsub("[^+-]", "", posid))
         )
}
