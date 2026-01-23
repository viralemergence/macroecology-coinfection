#' plot map of number of specimens per country
#'
#' @title plot_map_all
#'
#' @param map_data
#'
#' @return 
#' @export
plot_map_all <- function(map_data){
  
  # plot number of specimens per country
  map_data %>%
    ggplot() +
    geom_sf(aes(fill = n_specimen), color = "white") +
    coord_sf(xlim = c(-18, 137), ylim = c(-12, 52)) +
    scale_fill_distiller(name = "Number of \nspecimens", type = "seq",
                         palette = "PuBuGn", direction = 1, trans = "log", 
                         breaks = c(100, 500, 1000, 5000, 10000, 20000),
                         na.value = "gray80") +
    ggthemes::theme_map() +
    theme(legend.position = "right",
          legend.background = element_blank(),
          legend.key = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(color = "black", fill = NA))
  
}
