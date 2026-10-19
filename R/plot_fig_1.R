#' combine three plots to create Fig. 1
#'
#' @title plot_fig_1
#'
#' @param fig_heatmaps
#' @param fig_coinf_per_order
#' @param fig_map_pos_order
#'
#' @return 
#' @export
plot_fig_1 <- function(fig_heatmaps, fig_coinf_per_order, fig_map_pos_order){
  
  fig_1 <- cowplot::plot_grid(fig_heatmaps, fig_coinf_per_order,
                              fig_map_pos_order, labels = c("A", "B", "C"),
                              rel_heights = c(1.5, 1, 1.5),
                              ncol = 1, label_size = 12, label_y = c(1, 1.1, 1))
  
  return(fig_1)
}
