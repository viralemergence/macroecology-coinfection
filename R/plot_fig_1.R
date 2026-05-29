#' combine two plots to create Fig. 1
#'
#' @title plot_fig_1
#'
#' @param fig_coinf_per_taxa
#' @param fig_map_pos 
#'
#' @return 
#' @export
plot_fig_1 <- function(fig_coinf_per_taxa, fig_map_pos){
  
  fig_1 <- cowplot::plot_grid(fig_coinf_per_taxa, fig_map_pos,
                              labels = c('A', 'B'), ncol = 1, label_size = 14,
                              label_y = c(1, 1.04))
  
  return(fig_1)
}
