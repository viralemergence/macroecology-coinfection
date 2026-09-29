#' create two heatmaps of test effort 
#' virus group by host order and virus group combinations
#'
#' @title plot_heatmaps
#'
#' @param heatmap_data 
#'
#' @return 
#' @export

plot_heatmaps <- function(heatmap_data){
  
  # want the two heatmaps to have the same color scale
  tested_max = max(heatmap_data$order_effort$n_tested, 
                   heatmap_data$group_combos_effort$n_tested)
  
  # number of animals tested for each virus group, by host order
  p1 <- ggplot(heatmap_data$order_effort, 
               aes(x = host_order, y = virus_group_tested, fill = n_tested)) +
    geom_tile(color = "white", lwd = 1.5) +
    scale_fill_viridis(name = "Number of\nanimals tested",
                       option = "mako",
                       direction = -1,
                       begin = 0.2,
                       end = 0.9,
                       trans = "sqrt", 
                       breaks = c(100, 2500, 10000, 25000, 50000),
                       na.value = "white",
                       limits = c(0, tested_max)) +
    xlab("") + ylab("") +
    scale_y_discrete(limits = rev) +
    coord_fixed(ratio = 1) +
    theme_cowplot(font_size = 8) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  
  # make the heatmap
  p2 <- ggplot(heatmap_data$group_combos_effort, 
               aes(x = V1, y = V2, fill = n_tested)) +
    geom_tile(color = "white", lwd = 1.5) +
    scale_fill_viridis(name = "Number of\nanimals tested",
                       option = "mako",
                       direction = -1,
                       begin = 0.2,
                       end = 0.9,
                       trans = "sqrt", 
                       breaks = c(100, 2500, 10000, 25000, 50000),
                       na.value = "white",
                       limits = c(0, tested_max)) +
    xlab("") + ylab("") + 
    scale_y_discrete(limits = rev) +
    coord_fixed(ratio = 1) +
    theme_cowplot(font_size = 8) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # combine the plots with patchwork
  p1 + p2 +
    plot_annotation(tag_levels = "A") +
    theme(legend.position = "inside",
          legend.position.inside = c(0.65, 0.75))
    
}
