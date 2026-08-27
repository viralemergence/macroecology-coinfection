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
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
  V(g)$size_orig <- total_pos$n[match(V(g)$name, total_pos$virus)]
  V(g)$size_trans <- sqrt(V(g)$size_orig)
  V(g)$virus = as.character(
    total_pos$virus[match(V(g)$name, total_pos$virus)]) 
  
  # add virus targets for colors
  vir_targets <- pos_final[, c("virus", "virus_target_tested")] %>% 
    distinct()
 
  V(g)$virus_target_tested = V(g)$virus %>% 
    as_tibble() %>% 
    rename(virus = value) %>% 
    left_join(vir_targets) %>% 
    dplyr::select(virus_target_tested) %>% 
    unlist()
  
  V(g)$virus_target_tested <- as.character(V(g)$virus_target_tested)
  
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
  
  # so that each virus family will have a consistent color across panels
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
