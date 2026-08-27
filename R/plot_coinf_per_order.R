#' plot coinfection status per host order
#'
#' @title plot_coinf_per_order
#'
#' @param pos_final
#' @param pal 2 colors for coinfection status (single infection, coinfection)
#'
#' @return barchart with # of single infections vs coinfections by taxa group
#' @export
plot_coinf_per_order <- function(pos_final, pal = c("#9e9ac8", "#54278f")){
  
  pos_pcr_co <- pos_final %>% 
    dplyr::filter(!duplicated(predict_sample_id)) %>% 
    group_by(host_order, coinfect_status) %>% 
    count %>% 
    mutate(coinfect_status = factor(coinfect_status, 
                                    levels = c("single infection",
                                               "coinfection")))
  
  rm(pos_final)
  
  pos_pcr_co %>% 
    # only display orders with infections AND coinfections
    # excludes Carnivora, Charadriiformes, Galliformes, Passeriformes
    dplyr::filter(host_order %in% c("Anseriformes", "Chiroptera", 
                                    "Eulipotyphla", "Rodentia")) %>%
    arrange(-n) %>% 
    ggplot(aes(x = reorder(host_order, -n), y = n, fill = coinfect_status)) + 
    geom_bar(stat = "identity", position = position_dodge()) + 
    theme_bw(base_size = 16) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "inside",
          legend.position.inside = c(0.8, 0.8),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank()) + 
    scale_fill_manual(name = "Coinfection status", values = pal) +
    # improves readability, since bats outnumber other groups so much
    scale_y_sqrt(breaks = c(1, 50, 100, 250, 500, 1000, 1500, 2000)) +
    labs(x = 'Host order', y = 'Number of unique animals')

}