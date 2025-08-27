#' plot map of distribution of infected animals 
#' and whether they were singly infected or coinfected
#'
#' @title plot_map_pos
#'
#' @param map_data
#' @param pal 2 colors for infection status (single infection, coinfection)
#'
#' @return 
#' @export
plot_map_pos <- function(map_data, pal){
  
  ymin <- min(map_data$latitude, na.rm = T) - 5
  ymax <- max(map_data$latitude, na.rm = T) + 5
  xmin <- min(map_data$longitude, na.rm = T) - 5
  xmax <- max(map_data$longitude, na.rm = T) + 5
  
  # slow (lots of points)
  map_data %>% 
    # plots the coinfections last for ease of viewing
    arrange(coinfect_status) %>%
    ggplot() + 
    geom_sf(fill = "gray80", color = "white") + 
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
    # jittering helps illustrate how many points there are
    geom_jitter(aes(y = latitude, x = longitude, color = coinfect_status),
                width = 1.5, height = 1.5, size = 2, alpha = 0.8) +
    ggthemes::theme_map() + 
    scale_color_manual(name = "Infection status",
                      values = pal, 
                      na.translate = F) +
    theme(legend.position = "inside",
          legend.position.inside = c(0.55, 0.05),
          legend.background = element_blank(),
          legend.title = element_text(color = "black", size = 14),
          legend.text = element_text(color = "black", size = 12),
          legend.key = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(color = "black", fill = NA)) +
    guides(color = guide_legend(override.aes = list(size = 5)))
  
}
