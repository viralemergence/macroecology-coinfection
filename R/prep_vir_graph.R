#' prep graph data to plot coinfection network at the individual virus level
#'
#' @title prep_vir_graph
#'
#' @param MV
#' @param pos_final
#' @param pal color palette for virus families
#' @param sparse boolean--whether to drop nodes that don't coinfect
#'
#' @return 
#' @export
prep_vir_graph <- function(MV, pos_final, pal, sparse = TRUE){
  
  total_pos = pos_final %>% 
    group_by(virus) %>% 
    count %>% 
    mutate(number = "Positive")
  
  g <- graph_from_adjacency_matrix(MV, weighted = TRUE, mode = 'undirected')
  # set labels and degrees of vertices
  #E(g)$weight <- edge.betweenness(g)
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
  V(g)$size_orig <- total_pos$n[match(V(g)$name, total_pos$virus)]
  V(g)$size_trans <- sqrt(V(g)$size_orig)
  V(g)$virus = as.character(
    total_pos$virus[match(V(g)$name, total_pos$virus)]) 
  
  # add viral family for colors
  vir_fams <- pos_final[, c("virus", "viral_family")] %>% distinct()
 
  V(g)$viral_family = V(g)$virus %>% 
    as_tibble() %>% 
    rename(virus = value) %>% 
    left_join(vir_fams) %>% 
    dplyr::select(viral_family) %>% 
    unlist()
  
  V(g)$viral_family <- as.character(V(g)$viral_family)
  
  # only show labels of more central nodes
  for(i in 1:length(V(g))){
    
    if(V(g)$degree[i] < 4){
      V(g)$label[i] <- ""
    } 
  }
  
  if(sparse){
    isolated = which(degree(g)==0)
    g = igraph::delete_vertices(g, isolated)
  }
  
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
