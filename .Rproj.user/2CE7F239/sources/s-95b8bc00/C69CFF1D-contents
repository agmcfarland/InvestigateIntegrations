#' ROC to Heatmap
#'
#' @description Converts the output from `hotroc_compare_insertion_to_match()` into a `ggplot2()` heatmap.
#'
#' @param hot_roc_result The multidata object produced by `hotROCs::ROC.ORC()`.
#'
#' @return A `ggplot2()` plot object.
#'
#' @examples
#' format_hot_roc_result(hot_roc_result)
#'
#' @details Uses `format_hot_roc_result()` prior to creating ggplot object.
#'
#' @import ggplot2 dplyr
#'
#' @export
roc_to_heatmap <- function(
    hot_roc_result
){

  options(scipen=999) # ensures that 1000000 doesn't turn into 1e6, etc

  df_roc <- format_hot_roc_result(
    hot_roc_result = hot_roc_result
  )

  df_roc_pvals <- df_roc%>%
    dplyr::select(feature_window, heatmap_group,p_value,sig_p_value)

  df_roc <- df_roc%>%
    dplyr::select(feature_window, heatmap_group, ROC_value)

  ## Generate a basic heatmap of ROC results
  genomic_heatmap_plot <- ggplot2::ggplot()+
    ggplot2::geom_tile(
      data = df_roc,
      ggplot2::aes(x = heatmap_group, y = feature_window, fill = ROC_value))+
    ggplot2::geom_text(
      data = df_roc_pvals,
      ggplot2::aes(x = heatmap_group, y = feature_window, label = sig_p_value))+
    ggplot2::theme_classic()+
    ggplot2::theme(
      aspect.ratio = 1,
      axis.title = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank())+
    ggplot2::scale_fill_gradientn(colours=c('blue','grey90','red'),
                                  na.value = "transparent",
                                  breaks = c(0,0.25,0.5,0.75,1),
                                  labels = c(0,0.25,0.5,0.75,1),
                                  limits = c(0,1))+
    ggplot2::scale_y_discrete(expand = c(0,0))+
    ggplot2::scale_x_discrete(expand = c(0,0))+
    ggplot2::labs(fill='ROC')

  return(genomic_heatmap_plot)

}
