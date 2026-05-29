#' visualize a virus family coinfection network using ggraph package
#'
#' @title plot_vir_fam_network
#'
#' @param g igraph object
#' @param edge_width_lims vector of length 2 with min and max width of edges
#' @param node_size_range vector of length 2 with min and max size of nodes
#' @param node_size_lims either NULL or vector of length 2 to pass to limits argument of scale_size
#' @param show_fill_legend Boolean
#' 
#'
#' @return
#' @export
plot_vir_fam_network <- function(g, 
                                 edge_width_lims = NULL,
                                 node_size_range = c(1, 6), 
                                 node_size_lims = NULL,
                                 show_fill_legend = TRUE){
  
  g_tbl <- tidygraph::as_tbl_graph(g)
  
  edge_list <-
    g_tbl %>%
    tidygraph::activate(edges) %>%
    data.frame()
  
  
  net_out <- ggraph(g_tbl, layout = "linear", circular = TRUE) +
    geom_edge_link(aes(width = edge_list$weight), color = "gray70") +
    geom_edge_loop(aes(width = edge_list$weight,
                       direction = (from-2) * -360 / length(g)), 
                   color = "gray70") +
    scale_edge_width(name = "Number of \ncoinfections",
                     range = c(1, 4),
                     limits = edge_width_lims,
                     breaks = c(10, 50, 100),
                     labels = c(10, 50, 100)) + 
    geom_node_point(aes(size = size_trans, fill = factor(viral_family)), 
                    pch = 21) +
    scale_size(name = "Number of \ninfections", 
               range = node_size_range, 
               limits = node_size_lims, 
               breaks = c(10, 30, 50),
               labels = c(10^2, 30^2, 50^2)) +
    scale_fill_manual(name = "Viral family", values = V(g)$color) +
    scale_y_continuous(expand = expansion(mult = 0.1)) +
    scale_x_continuous(expand = expansion(mult = 0.1)) +
    theme_void() +
    theme_graph(plot_margin = margin(b = 0, l = 0, t = 0, r = 0)) +
    guides(fill = guide_legend(override.aes = list(size = 6)))
  
  # helps for later plotting
  if(show_fill_legend == FALSE){
    net_out <- net_out + guides(fill = "none")
  }
  
  return(net_out)
}
  