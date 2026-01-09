#' plot map of distribution of infected animals, colored by taxonomic group
#'
#' @title plot_map_pos_taxa
#'
#' @param map_data
#'
#' @return 
#' @export
plot_map_pos_taxa <- function(map_data){
  
  ymin <- min(map_data$latitude, na.rm = T)
  ymax <- max(map_data$latitude, na.rm = T)
  xmin <- min(map_data$longitude, na.rm = T)
  xmax <- max(map_data$longitude, na.rm = T)
  
  limits <- c(1, 5, 10, sqrt(250))
  labels <- as.character(limits^2) 
  
  map_data %>% 
    # plot the smallest points last for ease of viewing
    arrange(-size) %>%
    ggplot() + 
    geom_sf(fill = "gray80", color = "white") + 
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
    # jittering helps illustrate how many points there are
    geom_point(aes(y = latitude, x = longitude, color = taxa_group, size = size),
               position = position_jitter(width = 1, height = 1, seed = 42),
               alpha = 0.8) +
    ggthemes::theme_map() + 
    scale_size_continuous(name = "Number of specimens",
                          breaks = limits, labels = labels,
                          range = c(2, 8)) +
    scale_color_brewer(name = "Taxonomic group", type = "qual", palette = "Dark2", 
                       na.translate = F) +
    theme(legend.position = "bottom",
          legend.background = element_blank(),
          legend.box = "vertical",
          legend.title = element_text(color = "black", size = 14),
          legend.text = element_text(color = "black", size = 12),
          legend.key = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(color = "black", fill = NA)) +
    guides(color = guide_legend(override.aes = list(size = 5),
                                nrow = 1),
           size = guide_legend(nrow = 1))
  
}
