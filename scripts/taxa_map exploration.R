# experimenting with making a map that shows the geographic distribution of 
# samples colored by animal taxonomic group
tar_load(pos_final)

  
map_data <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  select(iso_a3, geometry)

inf_animals <- pos_final %>% 
  dplyr::filter(!duplicated(predict_sample_id)) %>% 
  select(predict_sample_id, country, taxa_group, latitude, longitude) %>% 
  # to avoid having multiple colors for taxa with only a few specimens
  mutate(
    taxa_group = forcats::fct_collapse(
      taxa_group,
      other = c("carnivores", "cattle/buffalo", "dogs", "goats/sheep",
                "other")),
    taxa_group = forcats::fct_relevel(
      taxa_group, "other", after = Inf)
    ) %>% 
  mutate(iso3c = countrycode(country, "country.name", "iso3c")) %>%
  # calculate sample sizes
  group_by(country, iso3c, taxa_group, latitude, longitude) %>% 
  summarise(n_specimen = n()) %>% 
  mutate(size = sqrt(n_specimen))

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

#----------------

# right now it's hard to distinguish some of the points in the smaller countries
# possible fixes:
  # split map into two panels (Africa and Asia)
  # group points that are near each other, creating fewer but larger points
ymin <- min(map_data$latitude, na.rm = T) - 5
ymax <- max(map_data$latitude, na.rm = T) + 5
xmin <- min(map_data$longitude, na.rm = T) - 5
xmax <- max(map_data$longitude, na.rm = T) + 5

limits <- c(1, 5, 10, sqrt(200))
labels <- as.character(limits^2) 

map_data %>% 
  # plots the smallest points last for ease of viewing
  arrange(-size) %>%
  ggplot() + 
  geom_sf(fill = "gray80", color = "white") + 
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  # jittering helps illustrate how many points there are
  # geom_jitter(aes(y = latitude, x = longitude, color = taxa_group),
  #             width = 1.5, height = 1.5, size = 2, alpha = 0.8) +
  geom_jitter(aes(y = latitude, x = longitude, color = taxa_group, size = size),
              width = 1.5, height = 1.5, alpha = 0.8) +
  ggthemes::theme_map() + 
  scale_size_continuous(name = "Number of\nspecimens",
                        breaks = limits, labels = labels,
                        range = c(2, 8)) +
  scale_color_brewer(name = "Taxonomic group", type = "qual", palette = "Dark2", 
                     na.translate = F) +
  theme(legend.position = "bottom",
        legend.background = element_blank(),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 12),
        legend.key = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA)) +
  guides(color = guide_legend(override.aes = list(size = 5),
                              nrow = 1))
  
