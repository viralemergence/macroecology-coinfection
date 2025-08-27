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
                              labels = c('A', 'B'), ncol = 1, label_size = 14)
  
  # using patchwork leaves a lot of white space 
  # fig_1 <- fig_coinf_per_taxa / fig_map_pos +
  #   plot_layout(heights = c(1, 1.5)) +
  #   plot_annotation(tag_levels = "A") & 
  #   theme(plot.tag = element_text(size  = 14))
  
  return(fig_1)
}
