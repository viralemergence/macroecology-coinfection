#' prep map data to show distribution of infected animals by host order
#'
#' @title prep_map_data_pos_order
#'
#' @param pos_final
#'
#' @return 
#' @export
prep_map_data_pos_order <- function(pos_final){
  
  map_data <- ne_countries(scale = "medium", returnclass = "sf") %>% 
    select(iso_a3, geometry)
  
  inf_animals <- pos_final %>% 
    dplyr::filter(!duplicated(predict_sample_id)) %>% 
    select(predict_sample_id, country, host_order, latitude, longitude) %>% 
    # round latitude and longitude a bit further to create fewer but larger points
    mutate(latitude = round(latitude, 0),
           longitude = round(longitude, 0)) %>%
    mutate(iso3c = countrycode(country, "country.name", "iso3c")) %>%
    # calculate sample sizes
    group_by(country, iso3c, host_order, latitude, longitude) %>% 
    summarise(n_animals = n()) %>% 
    mutate(size = sqrt(n_animals))
  
  # some points in Tanzania were given coordinates of (0,0)
  # re-assign those to the centroid of the country for plotting purposes
  tza <- map_data %>% 
    dplyr::filter(iso_a3 == "TZA")
  
  tza_centroid <- sf::st_centroid(tza)
  
  inf_animals <- inf_animals %>% 
    mutate(
      latitude = case_when(
        iso3c == "TZA" & latitude == 0 ~ tza_centroid$geometry[[1]][2],
        TRUE ~ latitude),
      longitude = case_when(
        iso3c == "TZA" & longitude == 0 ~ tza_centroid$geometry[[1]][1],
        TRUE ~ longitude)
    )
  
  map_data <- map_data %>% 
    left_join(., inf_animals, by = c("iso_a3" = "iso3c"))
  
  return(map_data)
  
}