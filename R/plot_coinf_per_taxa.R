#' plot coinfection status per taxa group
#'
#' @title plot_coinf_per_taxa
#'
#' @param pos_final
#' @param pal 2 colors for infection status (single infection, coinfection)
#'
#' @return barchart with # of single infections vs coinfections by taxa group
#' @export
plot_coinf_per_taxa <- function(pos_final, pal){
  
  pos_pcr_co <- pos_final %>% 
    dplyr::filter(!duplicated(predict_sample_id)) %>% 
    group_by(taxa_group, coinfect_status) %>% 
    count %>% 
    mutate(coinfect_status = factor(coinfect_status, 
                                    levels = c("single infection",
                                               "co-infection")))
  
  rm(pos_final)

  # calculate rates of co-infection  
  # pos_pcr_co %>%
  #   group_by(taxa_group) %>%
  #   summarise(total = sum(n)) %>%
  #   left_join(pos_pcr_co, .) %>%
  #   mutate(prop = n/total) %>%
  #   dplyr::filter(coinfect_status == "co-infection") %>%
  #   arrange(-prop)
  
  pos_pcr_co %>% 
    # only keep groups with single and coinfections
    dplyr::filter(taxa_group %in% c("bats", "rodents", "birds", "swine", 
                                    "shrews")) %>% 
    arrange(-n) %>% 
    ggplot(aes(x = reorder(taxa_group, -n), y = n, fill = coinfect_status)) + 
    geom_bar(stat = "identity", position = position_dodge()) + 
    theme_bw(base_size = 16) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "inside",
          legend.position.inside = c(0.8, 0.8),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank()) + 
    scale_fill_manual(name = "Infection status", values = pal) +
    # improves readability, since bats outnumber other groups so much
    scale_y_sqrt(breaks = c(1, 50, 100, 250, 500, 1000, 1500, 2000)) +
    labs(x = 'Taxonomic group', y = 'Number of unique animals')

}