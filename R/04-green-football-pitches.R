pacman::p_load("tidyverse", "here", "ggtext", "sf", "osmdata")

## GEOMETRIES ==================================================================

# Germany shape
de <- rnaturalearth::ne_countries(scale = 10, country = "Germany", returnclass = "sf")
st_crs(de)

#' Documentation: https://wiki.openstreetmap.org/wiki/DE:Tag:sport%3Dsoccer

# Retrieve data in vertical slices =============================================
de_bb <- st_bbox(de)

if (FALSE) {
  n_slices <- 10
  slice_boundaries <- seq(de_bb["ymin"], de_bb["ymax"], 
                          (de_bb["ymax"] - de_bb["ymin"]) / n_slices)
  
  pitches <- vector("list", n_slices)
  for (i in seq_len(n_slices)) {
    message(paste("Retrieving slice", i, "of", n_slices))
    bb_slice <- c("xmin" = de_bb["xmin"], "ymin" = slice_boundaries[i],
                  "xmax" = de_bb["xmax"], "ymax" = slice_boundaries[i + 1])
    
    pitches[[i]] <- opq(bbox = bb_slice, timeout = 1800) %>% 
      add_osm_feature(key = "leisure", value = "pitch") %>% 
      osmdata_sf()
  }
  filename <- paste0("osm-pitches-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".rds")
  write_rds(pitches, here("data", filename), compress = "gz")
} else {
  pitches <- read_rds(here("data", "osm-pitches-20221102-150559.rds"))
}

# Extract the polygons from the list
pitches_polygons <- map_dfr(pitches, pluck, "osm_polygons")

## Which sports?
table(pitches_polygons$sport)

# Keep football pitches
football_pitches_polygons <- pitches_polygons %>% 
  # sport values can contain multiple sports, e.g. "soccer;basketball"
  separate_rows(sport, sep = ";") %>% 
  filter(sport == "soccer")
# Filter to German borders
football_pitches_polygons_de <- st_filter(football_pitches_polygons, de)
# Get centroids
football_pitches_centroids_de <- st_centroid(football_pitches_polygons_de)

# Which types of surface?
table(football_pitches_centroids_de$surface)
football_pitches_centroids_de %>% 
  st_drop_geometry() %>% 
  count(surface, sort = TRUE) %>% 
  head(20)

# Map with football pitches
p <- ggplot(de) +
  geom_sf(fill = "white") +
  geom_sf(
    data = subset(football_pitches_centroids_de, surface == "grass"), 
    # data = football_pitches_centroids_de,
    color = "darkgreen", size = 1e-1, alpha = 0.7, shape = 16
  ) +
  coord_sf() +
  labs(
    title = "Football Grass Pitches",
    subtitle = "&laquo;Every Map is a Population Density Map&raquo;",
    caption = "Source: OpenStreetMap contributors
    (<span style='font-family:Courier New'>leisure=pitch</span> AND 
    <span style='font-family:Courier New'>sport=soccer</span> AND 
    <span style='font-family:Courier New'>surface=grass</span>).
    <br>
    Visualisation: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = colorspace::desaturate("#006400", 0.7), 
                                   fill = colorspace::desaturate("#006400", 0.7)),
    panel.background = element_rect(color = NA, fill = NA),
    text = element_text(color = "grey82"),
    plot.title = element_text(family = "Bebas Neue", color = "white", hjust = 0.5, size = 32),
    plot.subtitle = element_markdown(hjust = 0.5),
    plot.caption = element_markdown(hjust = 0.5, lineheight = 1.1),
    plot.margin = margin(rep(2, 4))
  )
ggsave(here("plots", "04-green-football-pitches-de.png"), dpi = 600, width = 5.75, height = 7)


