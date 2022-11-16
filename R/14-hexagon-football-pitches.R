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
    color = "darkgreen", size = 0.15, alpha = 0.9, shape = 16
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


## Make hexagon grid ===========================================================

de <- st_transform(de, crs = 3035)
initial <- de
initial$index_target <- 1:nrow(initial)
target <- st_geometry(initial)

# Create the grid of hexagons
grid <- st_make_grid(target,
                     cellsize = 25000,
                     crs = st_crs(initial),
                     what = "polygons",
                     square = FALSE # for hex, TRUE for squares
)
# Add index, transform list to dataframe
grid <- st_sf(id = 1:length(lengths(grid)), grid)
grid_de <- st_filter(grid, de, join = st_within)
football_pitches_centroids_de <- st_transform(football_pitches_centroids_de, crs = 3035)
st_crs(football_pitches_centroids_de)

football_pitches_centroids_de_reduced <- football_pitches_centroids_de %>% 
  select(osm_id, name, surface, geometry)

grid_pitches <- st_join(grid_de, football_pitches_centroids_de_reduced, 
                        join = st_contains)


grid_pitches %>%
  st_drop_geometry() %>% 
  count(surface, sort = TRUE) %>% View()

grid_pitches %>%
  st_drop_geometry() %>% 
  mutate(
    surface2 = case_when(
      surface %in% c("clay", "ash", "Ascheplatz", "Asche", "fine_gravel",
                     "gravel") ~ "clay",
      surface %in% c("grass", "turf", "Rasen", "rasen", "green", "Naturrasen", "lawn") ~ "grass",
      surface %in% c("artificial_grass", "artificial_turf", "synthetic_grass", "synthetic_turf", 
                     "artifical_grass", "kunstrasen", "Kunstrasen", "astroturf") ~ "artificial turf",
      TRUE ~ surface
    )
  ) %>% 
  count(surface2, sort = TRUE) %>% View()

grid_pitches_agg <- grid_pitches %>%
  st_drop_geometry() %>% 
  mutate(
    surface = case_when(
      surface %in% c("clay", "ash", "Ascheplatz", "Asche", "fine_gravel",
                     "gravel") ~ "clay",
      surface %in% c("grass", "turf", "Rasen", "rasen", "green", "Naturrasen", "lawn") ~ "grass",
      surface %in% c("artificial_grass", "artificial_turf", "synthetic_grass", "synthetic_turf", 
                     "artifical_grass", "kunstrasen", "Kunstrasen", "astroturf") ~ "artificial turf",
      TRUE ~ surface
    )
  ) %>% 
  count(id, surface, sort = TRUE) %>%
  filter(surface %in% c("grass", "artificial turf", "clay", "tartan")) %>% 
  complete(id = unique(grid_de$id), nesting(surface), fill = list(n = 0)) %>%
  right_join(grid, by = "id") %>%
  filter(!is.na(surface)) %>% 
  st_as_sf() 

p <- grid_pitches_agg %>%
  mutate(surface = fct_reorder(toupper(surface), -n)) %>% 
  ggplot() +
  geom_sf(fill = "white", color = "grey50", size = 0.1) +
  geom_sf(aes(fill = surface, alpha = n), 
          color = "grey50", size = 0.1) +
  scale_fill_manual(values = c("GRASS" = "darkgreen", "ARTIFICIAL TURF" = "#ABB621",
                               "CLAY" = "#56403F", "TARTAN" = "#AB7667")) +
  scale_alpha_continuous(range = c(0, 1.1), trans = "pseudo_log") +
  guides(fill = "none",
         alpha = guide_legend(title.position = "top")) +
  labs(
    title = "Football Pitch Surfaces",
    subtitle = "Frequency of surface types of football pitches in Germany on a
    25 km<sup>2</sup> grid.<br>Surface type coded by OpenStreetMap contributors.",
    caption = "Source: OpenStreetMap contributors. Visualisation: Ansgar Wolsing",
    alpha = "Number of pitches (pseudo-log)"
  ) +
  theme_void(base_family = "Roboto Condensed", base_size = 12) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(color = "grey98", fill = "grey98"),
    plot.margin = margin(rep(6, 4)), 
    text = element_text(color = "grey20", lineheight = 1.2),
    plot.title = element_text(
      color = "black", face = "bold", hjust = 0.5, size = 24,
      margin = margin(t = 4, b = 8), family = "Cabinet Grotesk"),
    plot.subtitle = element_markdown(
      hjust = 0.5, margin = margin(t = 2, b = 12)),
    plot.caption = element_markdown(hjust = 0.5),
    strip.text = element_text(face = "bold", size = 14, margin = margin(t = 4, b = 4)),
    strip.background = element_rect(color = "grey90", fill = "grey90"),
    panel.background = element_rect(color = "grey93", size = 1, fill = "grey96"),
    legend.position = "bottom"
  )

p1 <- p + 
  facet_wrap(vars(surface), nrow = 1)
ggsave(here("plots", "14-hexagons-football-pitch-surface-de-1-row.png"),
       width = 10, height = 10 * 9/16)

p2 <- p + 
  facet_wrap(vars(surface), nrow = 2)
ggsave(here("plots", "14-hexagons-football-pitch-surface-de-2-rows.png"),
       width = 8, height = 10)
