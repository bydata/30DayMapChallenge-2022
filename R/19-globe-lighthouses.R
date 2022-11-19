library(tidyverse)
library(sf)
library(osmdata)
library(here)

world <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")

result_file <- here("data", "osm-lighthouses.rds")
if (FALSE) {
  lighthouses_world <- opq(bbox = c(-180, -90, 180, 90), timeout = 1800) %>%
    # add_osm_feature(key = "building", value = "lighthouse") %>%
    add_osm_feature(key = "man_made", value = "lighthouse") %>%
    osmdata_sf()
  
  write_rds(lighthouses_world, result_file, compress = "gz")
} else {
  lighthouses_world <- read_rds(result_file)
}

# Number of lighthouses
nrow(lighthouses_world$osm_points)

crs <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=20"
lighthouses_world_points <- lighthouses_world$osm_points %>% 
  st_transform(crs = crs)

sphere_graticules <- st_graticule(ndiscr = 10000, margin = 10e-6) %>% 
  st_transform(crs = crs) %>%
  st_convex_hull() %>%
  summarise(geometry = st_union(geometry))


p <- ggplot() +
  geom_sf(data = sphere_graticules, fill = "#020417", size = 0.09) + 
  # geom_sf(
  #   data = world,
  #   fill = NA, size = 0.09, color = "grey32"
  # ) +
  ggfx::with_outer_glow(
    geom_point(
      data = lighthouses_world_points,
      aes(geometry = geometry),
      stat = "sf_coordinates",
      size = 0.005, color = "#faee66", shape = 16, alpha = 0.8),
    expand = 5, sigma = 5, color = "white"
  ) +
  coord_sf(crs = crs) +
  labs(
    title = "LIGHTHOUSES",
    subtitle = "Each dot represents a lighthouse",
    caption = "Data: OpenStreetMap contributors. Visualisation: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Poiret One") +
  theme(
    plot.background = element_rect(color = "#171717", fill = "#171717"),
    text = element_text(color = "grey98"),
    plot.title = element_text(
      family = "Codystar", color = "#faee66", hjust = 0.5, size = 42),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    plot.margin = margin(rep(4, 4))
  )
ggsave(here("plots", "19-globe-lighthouses.png"), dpi = 1000, width = 6.5, height = 7)
