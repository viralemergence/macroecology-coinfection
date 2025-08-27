#' combine 3 panel plots to create Fig. 2
#'
#' @title plot_fig_2
#'
#' @param ggraph_network_panel
#' @param model_coefs_panel 
#'
#' @return 
#' @export
plot_fig_2 <- function(ggraph_network_panel, model_coefs_panel){
  
  fig_2 <- model_coefs_panel | plot_spacer() | ggraph_network_panel
  
  rm(ggraph_network_panel, model_coefs_panel)
  
  fig_2 <- fig_2 + plot_layout(widths = c(1, 0.1, 1.2))
  
  fig_2 <- fig_2 + plot_layout(tag_level = "new") +
    plot_annotation(tag_levels = list(c("A", "C", "E", "B", "D", "F"))) & 
    theme(plot.tag = element_text(size = 24),
          plot.tag.position = "topleft")
  
  return(fig_2)
}