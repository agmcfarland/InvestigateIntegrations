#' Format hotROCs Result
#'
#' @description Reformats the ROC output of `hotroc_compare_insertion_to_match()` into a `data.frame()` object.
#'
#' @param hot_roc_result The multidata object produced by `hotROCs::ROC.ORC()`.
#'
#' @return A `data.frame` object with the `feature_window`, the `heatmap_group` it belongs to, the calculated `p_value`, the `sig_p_value`, and the `ROC_value`
#' \describe{
#' \item{feature_window}{The genomic feature sampled and window size that was used.}
#' \item{heatmap_group}{The grouping used.}
#' \item{p_value}{The p-value calculated by `hotROCs::ROC.ORC()`.}
#' \item{sig_p_value}{Star representation of p-value.}
#' \item{ROC_value}{The ROC value calculated by `hotROCs::ROC.ORC()`.}}
#'
#' @examples
#' format_hot_roc_result(hot_roc_result)
#'
#' @import tibble tidyr dplyr
#'
#' @export
format_hot_roc_result <- function(hot_roc_result){
  options(scipen=999)
  df_roc <- data.frame(hot_roc_result$ROC)%>%
    tibble::rownames_to_column('feature_window')%>%
    tidyr::pivot_longer(cols=-feature_window, names_to='heatmap_group',values_to='ROC_value')

  df_roc_pval <- data.frame(hot_roc_result$pvalues$np)%>%
    tibble::rownames_to_column('feature_window')%>%
    tidyr::pivot_longer(cols=-feature_window, names_to='heatmap_group',values_to='p_value')%>%
    dplyr::mutate(
      sig_p_value = dplyr::case_when(
        p_value <= 0.001 ~ '***',
        p_value <= 0.01 ~ '**',
        p_value <= 0.05 ~ '*',
        TRUE ~ ''
      )
    )
  df_roc <-
    merge(df_roc_pval,
          df_roc,
          by = c('feature_window', 'heatmap_group'))

  return(df_roc)
}
