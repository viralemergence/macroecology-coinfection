#' prep map data to show specimen distribution by country
#'
#' @title prep_map_data_all
#'
#' @param pcr
#'
#' @return 
#' @export
prep_map_data_all <- function(pcr){
  
  unique_specimens <- pcr %>% 
    dplyr::filter(!duplicated(specimen_id)) %>% 
    select(specimen_id, country, taxa_group, latitude, longitude) %>% 
    mutate(iso3c = countrycode(country, "country.name", "iso3c")) 
  
  specimens_per_country <- unique_specimens %>% 
    group_by(iso3c) %>% count %>% 
    left_join(unique_specimens, .) %>% 
    group_by(iso3c) %>% 
    summarise(n_specimen = mean(n), 
              latitude = mean(latitude),
              longitude = mean(longitude)) %>% 
    mutate(size = sqrt(n_specimen/100))
  
  map_data <- ne_countries(scale = "medium", returnclass = "sf") %>% 
    select(iso_a3, geometry) %>% 
    left_join(., specimens_per_country, by = c("iso_a3" = "iso3c"))
  
  return(map_data)

}
