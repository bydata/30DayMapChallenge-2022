library(tidyverse)
library(sf)
library(osmdata)
library(here)

world <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")

lighthouses_world <- opq(bbox = c(-180, -90, 180, 90), timeout = 1800) %>%
  # add_osm_feature(key = "building", value = "lighthouse") %>%
  add_osm_feature(key = "man_made", value = "lighthouse") %>%
  osmdata_sf()

write_rds(lighthouses_world, here("data", "osm-lighthouses.rds"), compress = "gz")

nrow(lighthouses_world$osm_points)

p <- ggplot() +
  geom_sf(
    data = world,
    fill = NA, size = 0.075, color = "grey28"
  ) +
  ggfx::with_outer_glow(
    geom_point(
      data = lighthouses_world$osm_points,
      aes(geometry = geometry),
      stat = "sf_coordinates",
      size = 0.005, color = "#faee66", shape = 16, alpha = 0.8),
    expand = 5, sigma = 5, color = "white"
  ) +
  coord_sf(crs = "ESRI:102017") +
  labs(
    title = "LIGHTHOUSES",
    subtitle = "Each dot represents a lighthouse",
    caption = "Data: OpenStreetMap contributors. Visualisation: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Poiret One") +
  theme(
    plot.background = element_rect(color = "grey8", fill = "grey8"),
    text = element_text(color = "grey98"),
    plot.title = element_text(family = "Codystar", color = "#faee66", hjust = 0.5, size = 36),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    plot.margin = margin(rep(4, 4))
  )
ggsave(here("plots", "19-globe-lighthouses.png"), dpi = 1000, width = 6, height = 7)
