#' plot map of number of specimens per country
#'
#' @title plot_map_all
#'
#' @param map_data
#'
#' @return 
#' @export
plot_map_all <- function(map_data){
  
  limits <- c(2,4,6,8)
  labels <- as.character(limits^2*100) 
  
  ymin <- min(map_data$latitude, na.rm = T) - 5
  ymax <- max(map_data$latitude, na.rm = T) + 5
  xmin <- min(map_data$longitude, na.rm = T) - 5
  xmax <- max(map_data$longitude, na.rm = T) + 5
  
  # plot number of specimens per country
  map_data %>%
    # plots the larger circles first for ease of viewing
    arrange(-size) %>%
    ggplot() +
    geom_sf(fill = "gray80", color = "white") +
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
    geom_point(aes(y = latitude, x = longitude, size = size),
               fill = "#6CA6CD", color = "white", pch = 21, alpha = 0.8) +
    ggthemes::theme_map() +
    scale_size_continuous(name = "Number of\nspecimens",
                          breaks = limits, labels = labels,
                          range = c(2, 8)) +
    theme(legend.position = "inside",
          legend.position.inside = c(0.52, -0.01),
          legend.background = element_blank(),
          legend.key = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(color = "black", fill = NA))
  
}
