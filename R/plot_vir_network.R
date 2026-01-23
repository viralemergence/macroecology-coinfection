#' visualize an individual virus coinfection network using ggraph package
#'
#' @title plot_vir_network
#'
#' @param g igraph object
#' 
#'
#' @return
#' @export
plot_vir_network <- function(g){
  
  g_tbl <- tidygraph::as_tbl_graph(g)
  
  edge_list <-
    g_tbl %>%
    tidygraph::activate(edges) %>%
    data.frame()
  
  # this ensures the viral family colors are consistent 
  # with the family coinfection network plots
  pal <-
    g_tbl %>%
    tidygraph::activate(nodes) %>%
    data.frame() %>% 
    arrange(viral_family) %>% 
    distinct(viral_family, .keep_all = T) %>% 
    pull(color)
  
  net_out <- ggraph(g_tbl, layout = "kk") +
    geom_edge_link(aes(width = edge_list$weight), color = "gray70") +
    scale_edge_width(name = "Number of \ncoinfections") + 
    geom_node_point(aes(size = size_trans,
                        fill = factor(viral_family)), 
                    pch = 21) +
    geom_node_label(aes(label = label),  
                    repel = TRUE, max.overlaps = 20, show.legend = FALSE, 
                    family = "sansserif",
                    alpha = 0.7, vjust = 0.8, hjust = 0.8) +
    scale_size(name = "Number of \ninfections", 
               range = c(2, 18), 
               breaks = c(5, 10, 15, 20),
               labels = c(5^2, 10^2, 15^2, 20^2)) +
    scale_fill_manual(name = "Viral family", values = pal) +
    scale_y_continuous(expand = expansion(mult = 0.1)) +
    scale_x_continuous(expand = expansion(mult = 0.1)) +
    theme_void() +
    theme_graph(plot_margin = margin(b = 0, l = 0, t = 0, r = 0)) +
    guides(fill = guide_legend(override.aes = list(size = 10)))
  
  return(net_out)
}
