library(sf)
library(tidyverse)
library(osmdata)

de <- rnaturalearth::ne_countries(scale = 10, country = "Germany", returnclass = "sf")

places <- c("Moers", "Duisburg", "Hannover", "Mannheim", "Düsseldorf", "Köln")
places_bb <- map(paste(places, "Deutschland", sep = ", "), getbb)
places_bb

get_center <- function(bbox) {
  x = mean(c(bbox["x", "min"], bbox["x", "max"]))
  y = mean(c(bbox["y", "min"], bbox["y", "max"]))
  list(x = x, y = y)
}

places_coords <- map_df(places_bb, get_center) %>% 
  add_column(place = places)
places_coords

places_sf <- st_as_sf(places_coords, coords = c("x", "y"), crs = st_crs(de))

ggplot() +
  geom_sf(data = de, fill = "#E9EB87") +
  geom_sf(data = places_sf, col = "white", shape = 21, fill = "black") +
  ggrepel::geom_text_repel(
    data = places_coords, aes(x, y, label = place), family = "Chivo",
    point.padding = 3) +
  labs(
    title = "Places I've lived",
    caption = "Source: OpenStreetMap contributors"
  ) +
  theme_void(base_family = "Lato") +
  theme(plot.background = element_rect(color = "white", fill = "white"),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.margin = margin(rep(2, 4))
        )

ggsave(here::here("plots", "13-5min-map.png"), width = 4, height = 6)
