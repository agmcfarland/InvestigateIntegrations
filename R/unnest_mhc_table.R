#' Unnest MHC Table
#'
#' This function takes an AAvengeR MHC table with a `posids` column as a list and unnests it. The `posids` column is renamed to `posid`.
#'
#' @param aavenger_mhc A data frame representing the AAvengeR MHC multihits cluster table, which must contain a `posids` column of class `list`.
#' @return A data frame with the `posids` column unnested and renamed to `posid`.
#' @details This function takes an AAvengeR MHC table with a `posids` column as a list and unnests it. The `posids` column is renamed to `posid`.
#' @import dplyr
#' @import tidyr
#' @import testthat
#' @export
unnest_mhc_table <- function(aavenger_mhc) {

  testthat::expect_equal(class(aavenger_mhc$posids), 'list', info = 'AAvengeR multihits cluster table must have posids as a list')

  aavenger_mhc <- aavenger_mhc %>%
    tidyr::unnest(posids) %>%
    dplyr::rename(posid = posids)

  return(aavenger_mhc)
}
