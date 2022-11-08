pacman::p_load("tidyverse", "here", "glue", "ggtext", "colorspace", "scico",
               "sf", "osmdata", "gganimate")

## GEOMETRIES ==================================================================

crs <- "EPSG:4326"

## Shape Cologne
shape_cgn <- getbb("Cologne, Germany", format_out = "sf_polygon")
st_crs(shape_cgn) <- crs

# Streets
relevant_highway_values <- c("motorway", "road", "primary", "secondary", 
                             "tertiary", "residential")
osm_key_value_pairs <- paste0("\"highway\"", "=", "\"", relevant_highway_values, "\"")
highway_features <- opq(bbox = shape_cgn, timeout = 1200) %>%
  add_osm_features(features = osm_key_value_pairs) %>%
  osmdata_sf()

# Charging stations
charging_features <- opq(bbox = shape_cgn) %>%
  add_osm_feature(key = "amenity", value = "charging_station") %>%
  osmdata_sf()

charging_features$osm_points %>%
  st_drop_geometry() %>%
  count(operator, sort = TRUE)

# operator column needs some cleanup of names
charging_features_points <- charging_features$osm_points %>%
  mutate(operator = replace_na(operator, "Unknown"),
         operator = case_when(
           str_detect(operator, "(?i)rhein\\s?energ(ie|y)") ~ "RheinEnergie",
           str_detect(operator, "Tesla") ~ "Tesla",
           str_detect(operator, "(?i)innogy") ~ "Innogy",
           str_detect(operator, "^EnBW") ~ "EnBW",
           str_detect(operator, "Currenta?") ~ "Currenta GmbH & Co. OHG",
           str_detect(operator, "(?i)^Aldi") ~ "Aldi Süd",
           TRUE ~ operator
         ),
         operator_grp = fct_lump(operator, n = 5))  
#Stad(t)werke Köln GmbH

charging_features_points %>%
  st_drop_geometry() %>%
  count(operator, sort = TRUE)

charging_features_points %>%
  st_drop_geometry() %>%
  count(operator_grp, sort = TRUE)

st_crs(shape_cgn)
st_crs(charging_features_points)
charging_features_cgn <- st_intersection(shape_cgn, charging_features_points)

ggplot() +
  geom_sf(data = shape_cgn,
          fill = "grey21") +
  geom_sf(data = charging_features_cgn,
          aes(col = operator_grp), shape = 15)

highway_features$osm_lines %>%
  st_drop_geometry() %>%
  count(highway, sort = TRUE)

highway_features_lines <-
  highway_features$osm_lines # %>%
# filter(highway %in% relevant_highway_values)

highway_features_lines_simplified <- st_simplify(highway_features_lines)

# Limit streets to Cologne shape
highway_features_lines_cgn <- st_filter(highway_features_lines_simplified, shape_cgn)
write_rds(highway_features_lines_cgn, here("data", "highway_features_lines_cgn.rds"))
highway_features_lines_cgn <- read_rds(here("data", "highway_features_lines_cgn.rds"))

p <- ggplot() +
  geom_sf(data = shape_cgn,
          fill = "grey90") +
  geom_sf(data = highway_features_lines_cgn, size = 0.2, col = "grey68") +
  geom_sf(data = filter(highway_features_lines_cgn, highway == "motorway"),
          size = 0.5, col = "grey68") +
  geom_sf(data = charging_features_cgn,
          aes(col = operator_grp),
          shape = 16, size = 2) +
  paletteer::scale_color_paletteer_d("jcolors::pal3") +
  labs(fill = "Operator") +
  theme_void() +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    text = element_text(family = "Lato"),
        legend.position = c(0.8, 0.8))
ggsave(here("plots", "08-osm-ev-charging-stations-cgn.png"), dpi = 200, width = 10, height = 8)
