#' make 3 row panel plot of networks
#'
#' @title plot_vir_fam_network_panel
#'
#' @param g igraph object
#' @param g_bats
#' @param g_rodents
#' @param add_stars boolean. add significance stars or not
#' 
#'
#' @return
#' @export
plot_vir_fam_network_panel <- function(g, g_bats, g_rodents, add_stars){
  
  # ensures consistency of edge widths across the network plots
  edge_weights <- c(E(g)$weight, E(g_bats)$weight, E(g_rodents)$weight)
  edge_width_lims <- c(floor(min(edge_weights)), ceiling(max(edge_weights)))
  
  # ensures consistency of node sizes across the network plots
  node_sizes <- c(V(g)$size_trans, V(g_bats)$size_trans, V(g_rodents)$size_trans)
  node_size_lims <- c(floor(min(node_sizes)), ceiling(max(node_sizes)))
  
  p1 <- plot_vir_fam_network(
    g,
    edge_width_lims = edge_width_lims,
    node_size_range = c(2, 9),
    node_size_lims = node_size_lims,
    show_fill_legend = TRUE)
  
  p2 <- plot_vir_fam_network(
    g_bats,
    edge_width_lims = edge_width_lims,
    node_size_range = c(2, 9),
    node_size_lims = node_size_lims,
    show_fill_legend = FALSE) +
    labs(tag = "C")
  
  p3 <- plot_vir_fam_network(
    g_rodents,
    edge_width_lims = edge_width_lims,
    node_size_range = c(2, 9),
    node_size_lims = node_size_lims,
    show_fill_legend = FALSE) +
    labs(tag = "E")
  
  if(add_stars){
    
    star_size = 10
    
    p1 <- p1 +     
      geom_text(x = -0.75, y = 0.7, label = "*", size = star_size) +
      geom_text(x = -0.05, y = 0.45, label = "*", size = star_size) +
      geom_text(x = 0.15, y = -0.17, label = "*", size = star_size)
    
    p2 <- p2 + 
      geom_text(x = -0.85, y = 0.48, label = "*", size = star_size) +
      geom_text(x = -0.35, y = 0.45, label = "*", size = star_size) +
      geom_text(x = 0.15, y = -0.15, label = "*", size = star_size)
    
    p3 <- p3 +
      geom_text(x = 1.65, y = -0.37, label = "*", size = star_size)
  }
  
  # combine everything
  network_panel <- (p1 / plot_spacer() / p2 / plot_spacer() / p3) 
  
  network_panel <- network_panel + plot_layout(heights = c(1, 0.17, 1, 0.17, 1))
  
  network_panel <- network_panel + 
    plot_layout(guides = "collect") & 
    theme(legend.position = "right",
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10),
          legend.background = element_blank())
  
  return(network_panel)
}
