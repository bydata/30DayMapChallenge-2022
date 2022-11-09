pacman::p_load("tidyverse", "here", "glue", "ggtext", "colorspace", "scico",
               "sf", "osmdata", "gganimate")

## GEOMETRIES ==================================================================

crs <- "EPSG:4326"

get_osm_data <- function(place_name) {
  ## Shape 
  shape <- getbb(place_name, format_out = "sf_polygon", limit = 1)
  if ("list" %in% class(shape) & "multipolygon" %in% names(shape)) {
    shape <- shape$multipolygon
  }
  st_crs(shape) <- crs
  
  # Streets
  relevant_highway_values <- c("motorway", "road", "primary", "secondary", 
                               "tertiary", "residential")
  osm_key_value_pairs <- paste0("\"highway\"", "=", "\"", relevant_highway_values, "\"")
  highway_features <- opq(bbox = st_bbox(shape), timeout = 1200) %>%
    add_osm_features(features = osm_key_value_pairs) %>%
    osmdata_sf()
  # Limit streets to place shape
  highway_features_lines_simplified <- st_simplify(highway_features$osm_lines)
  highway_features_lines_simplified <- st_filter(highway_features_lines_simplified, shape)
  
  # Charging stations
  charging_features <- opq(bbox = st_bbox(shape)) %>%
    add_osm_feature(key = "amenity", value = "charging_station") %>%
    osmdata_sf()
  
  charging_features_points <- st_intersection(shape, charging_features$osm_points)
  
  return(list(shape = shape,
              highway_features = highway_features_lines_simplified,
              charging_stations = charging_features_points
              ))  
}

places <- c("Berlin", "Hamburg", "Munich", "Cologne", "Frankfurt", "Stuttgart",
            "Düsseldorf", "Dortmund", "Essen") %>% 
  paste("Germany", sep = ", ")

# foo <- get_osm_data("Berlin, Germany")

osm_results <- map(places, function(x) {
  message(paste("Retrieving OSM data for", x))
  result <- get_osm_data(x)
  filename <- paste0("osm-charging-stations-", x, ".rds")
  write_rds(result, here("data", filename))
  return(result)
})
osm_results <- set_names(osm_results, places)


# Capacity
osm_results2 <- map(osm_results, function(x) {
  x[["charging_stations"]] <- pluck(x, "charging_stations") %>% 
    mutate(capacity = as.numeric(capacity),
           capacity = replace_na(capacity, 1),
           capacity_grp = case_when(
             capacity == 1 ~ "1",
             capacity >=2 & capacity <= 4 ~ "2-4",
             capacity >= 5 & capacity <= 10 ~ "5-10",
             capacity > 10 ~ "10+"
           ))
  x
})

# Adapt Hamburg shape - remove Neuwerk island
osm_results2[["Hamburg, Germany"]]$shape <- osm_results2[["Hamburg, Germany"]]$shape %>% 
  st_crop(c(xmin = 9.5, ymin = 53.3, xmax = 10.3, ymax = 53.8)) 

for (result in osm_results2) {
  p <- ggplot() +
    geom_sf(data = result$shape,
            fill = "grey21")+
    geom_sf(data = result$highway_features,
            col = "grey90", size = 0.1) +
    geom_sf(data = result$charging_stations,
            aes(size = capacity),
            col = "#87F6FF", fill = alpha("#87F6FF", 0.5), stroke = 0.1, shape = 21) +
    scale_size_continuous(breaks = c(1, 5, 10, 50, 100))
  print(p)
}

p <- vector("list", length(osm_results2))
for (i in seq_along(osm_results2)) {
  result <- osm_results2[[i]]
  place_name <- names(osm_results2[i])
  p[[i]] <- ggplot() +
    geom_sf(data = result$shape,
            fill = "grey40", size = 0.1) +
    geom_sf(data = result$highway_features, size = 0.1, col = "grey68") +
    geom_sf(data = filter(result$highway_features, highway == "primary"),
            size = 0.2, col = "grey68") +
    geom_sf(data = filter(result$highway_features, highway == "motorway"),
            size = 0.5, col = "grey68") +
    geom_sf(data = result$charging_stations,
            # aes(size = capacity),
            col = "#87F6FF", fill = alpha("#87F6FF", 0.5), stroke = 0.1, shape = 21) +
    scale_size_continuous(breaks = c(1, 5, 10, 50, 100)) +
    labs(
      title = str_extract(place_name, ".+?,") %>% str_remove(",") %>% toupper()
    ) +
    theme_void(base_family = "Lato") +
    theme(
      plot.background = element_rect(color = "grey99", fill = "grey99"),
      text = element_text(),
      plot.title = element_text(face = "bold", hjust = 0.5, color = "grey36",
                                size = 12),
      legend.position = c(0.8, 0.8))
  # print(here("plots", paste0("08-osm-ev-charging-stations-", place_name, ".png")))
  # ggsave(here("plots", paste0("08-osm-ev-charging-stations-", place_name, ".png")), 
  #        dpi = 200, width = 10, height = 8)
}

library(patchwork)

p_combined <- (p[[1]] + p[[2]] + p[[3]]) /  
  (p[[4]] + p[[5]] + p[[6]]) /  
  (p[[7]] + p[[8]] + p[[9]]) +
  plot_annotation(
    title = "EV Charging Stations in the Largest Cities in Germany",
    caption = "Note: City shapes are not equally scaled<br>
    Source: OpenStreetMap contributors. Visualisation: Ansgar Wolsing",
    theme = theme(
      plot.title = element_markdown(hjust = 0.5, size = 18, face = "bold"),
      plot.caption = element_markdown(hjust = 0.5, lineheight = 1.1))
  ) &
  theme(
    plot.background = element_rect(color = "grey99", fill = "grey99"),
    text = element_text(family = "Lato")
  )
ggsave(here("plots", "08-osm-ev-charging-stations-germany-cities.png"), width = 8, height = 9)
