#' prep graph data to plot coinfection network at the virus target level
#'
#' @title prep_vir_target_graph
#'
#' @param M
#' @param total_pos
#' @param pal color palette for virus targets
#'
#' @return 
#' @export
prep_vir_target_graph <- function(M, total_pos, pal){
  
  g <- graph_from_adjacency_matrix(M, weighted = TRUE, diag = T,
                                   mode = 'undirected')
  
  # set labels and degrees of vertices
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
  V(g)$size_orig <- total_pos$n[match(V(g)$name, total_pos$virus_target_tested)]
  V(g)$size_trans <- sqrt(V(g)$size_orig)
  V(g)$virus_target_tested = as.character(
    total_pos$virus_target_tested[match(V(g)$name, total_pos$virus_target_tested)]) 
  V(g)$label.cex = 1.2
  
  # so that each virus target will have a consistent color across panels
  node_colors <- data.frame(
    targets = c("Arenaviruses", "Coronaviruses", "Filoviruses", "Flaviviruses", 
             "Hantaviruses", "Influenzas", "Paramyxoviruses", "Rhabdoviruses"),
    colors = pal)
  V(g)$color = V(g)$virus_target_tested
  for (i in 1:length(V(g))){
    V(g)$color[i] <- node_colors$colors[which(node_colors$targets==V(g)$color[i])]
    }
  
  return(g)
  
}
