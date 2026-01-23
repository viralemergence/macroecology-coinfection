#' prep graph data to plot coinfection network at the virus family level
#'
#' @title prep_vir_fam_graph
#'
#' @param M
#' @param total_pos
#' @param viral_fams
#' @param pal color palette
#'
#' @return 
#' @export
prep_vir_fam_graph <- function(M, total_pos, viral_fams, pal){
  
  g <- graph_from_adjacency_matrix(M, weighted = TRUE, diag = T,
                                   mode = 'undirected')
  # set labels and degrees of vertices
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
  V(g)$size_orig <- total_pos$n[match(V(g)$name, total_pos$viral_family)]
  V(g)$size_trans <- sqrt(V(g)$size_orig)
  V(g)$viral_family = as.character(
    total_pos$viral_family[match(V(g)$name, total_pos$viral_family)]) 
  V(g)$label.cex = 1.2
  
  
  # so that each viral family will have a consistent color across panels
  node_colors <- data.frame(
    fams = c("Arenaviridae", "Coronaviridae", "Filoviridae", "Flaviviridae",
             "Hantaviridae", "Orthomyxoviridae", "Paramyxoviridae", 
             "Rhabdoviridae"),
    colors = pal)
  V(g)$color = V(g)$viral_family
  for (i in 1:length(V(g))){
    V(g)$color[i] <- node_colors$colors[which(node_colors$fams==V(g)$color[i])]
    }
  
  return(g)
  
}
