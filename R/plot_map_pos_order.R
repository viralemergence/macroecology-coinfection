#' plot map of distribution of infected animals, colored by host order
#'
#' @title plot_map_pos_order
#'
#' @param map_data
#'
#' @return 
#' @export
plot_map_pos_order <- function(map_data){
  
  ymin <- min(map_data$latitude, na.rm = T)*1.25
  ymax <- max(map_data$latitude, na.rm = T)*1.05
  xmin <- min(map_data$longitude, na.rm = T)
  xmax <- max(map_data$longitude, na.rm = T)
  
  limits <- c(1, 5, 10, sqrt(250))
  labels <- as.character(limits^2) 
  
  map_data %>% 
    dplyr::mutate(
      host_order = case_when(
        host_order %in% c("Carnivora", "Charadriiformes", "Galliformes", 
                          "Passeriformes") ~ "Other",
        TRUE ~ host_order),
      host_order = fct_relevel(host_order, "Other", after = Inf)) %>% 
    # plot the smallest points last for ease of viewing
    arrange(-size) %>%
    ggplot() + 
     geom_sf(fill = "gray80", color = "white") + 
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
    geom_point(aes(y = latitude, x = longitude, fill = host_order, size = size), 
               alpha = 0.8, pch = 21, color = "black", 
               position = position_jitter(width = 1, height = 1, seed = 42)) +
    scale_fill_manual(name = "Host order",
                      values = ltc::ltc("expevo", 6)[c(5,3,2,1,6)],
                      na.translate = F) +
    ggthemes::theme_map() + 
    scale_size_continuous(name = "Number of animals infected with ≥ 1 virus",
                          breaks = limits, labels = labels,
                          range = c(2, 8)) +
    theme(legend.position = "bottom",
          legend.background = element_blank(),
          legend.box = "vertical",
          legend.title = element_text(color = "black", size = 14),
          legend.text = element_text(color = "black", size = 12),
          legend.key = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(color = "black", fill = NA)) +
    guides(fill = guide_legend(override.aes = list(size = 5),
                                nrow = 1),
           size = guide_legend(nrow = 1))
  
}