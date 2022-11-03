library(tidyverse)
library(sf)
library(osmdata)
library(here)

de <- rnaturalearth::ne_countries(scale = 10, country = "Germany", returnclass = "sf")

if (FALSE) {
  rivers <- opq(bbox = st_bbox(de), timeout = 1800) %>%
    add_osm_feature(key = "waterway", value = "river") %>%
    osmdata_sf()
  write_rds(rivers, here("data", "osm-rivers.rds"), compress = "gz")
} else {
  rivers <- read_rds(here("data", "osm-rivers.rds"))
}

rivers_multilines_prep <- rivers$osm_multilines %>% 
  mutate(
    # full length (including waterway outside Germany)
    full_length = st_length(geometry),
    full_length_km = as.numeric(full_length) / 1000,
    name_consolidated = ifelse(!is.na(name.en) & name.en != "", name.en, name)) %>% 
  arrange(-full_length) %>% 
  st_intersection(st_buffer(de, 100)) %>% 
  # calculate the river length within Germany
  mutate(
    de_length = st_length(geometry),
    de_length_km = as.numeric(de_length) / 1000,
  ) %>% 
  select(osm_id, name, name.en, name_consolidated, width, contains("_length"), geometry)

rivers_multilines_prep %>% 
  st_drop_geometry() %>% 
  head(10)


p1 <- rivers_multilines_prep %>% 
  ggplot(aes(color = de_length_km)) +
  geom_sf(aes(size = de_length_km)) +
  geom_sf_label(
    data = slice_max(rivers_multilines_prep, order_by = de_length_km, n = 8),
    aes(label = name_consolidated),
    fill = alpha("grey22", 0.7), family = "Chivo", label.size = 0.1
    ) +
  scico::scale_color_scico(palette = "nuuk") +
  scale_size_continuous(range = c(0.1, 0.8)) +
  guides(size = "none",
         color = "none") +
  labs(
    title = "Rivers of Germany",
    subtitle = "River courses coloured by their length within Germany",
    caption = "Data: OpenStreetMap contributors. Visualisation: Ansgar Wolsing"
  ) + 
  theme_void(base_family = "Montserrat", base_size = 12) +
  theme(
    plot.background = element_rect(color = "grey8", fill = "grey8"),
    panel.background = element_rect(color = NA, fill = NA),
    text = element_text(color = "grey95"),
    plot.title = element_text(color = "#FEFEFE", family = "Chivo", 
                              size = 28, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    plot.margin = margin(rep(4, 4))
  )
ggsave(here("plots", "02-lines-rivers-de-without-shape.png"), dpi = 600, width = 7, height = 8)

p2 <- rivers_multilines_prep %>% 
  ggplot(aes(color = de_length_km)) +
  geom_sf(
    data = de,
    aes(geometry = geometry), fill = "grey16", color = "grey70", size = 0.01) +
  geom_sf(aes(size = de_length_km)) +
  geom_sf_label(
    data = slice_max(rivers_multilines_prep, order_by = de_length_km, n = 8),
    aes(label = name_consolidated),
    fill = alpha("grey22", 0.7), family = "Chivo", label.size = 0.1
  ) +
  scico::scale_color_scico(palette = "nuuk") +
  scale_size_continuous(range = c(0.1, 0.8)) +
  guides(size = "none",
         color = "none") +
  labs(
    title = "Rivers of Germany",
    subtitle = "River courses coloured by their length within Germany",
    caption = "Data: OpenStreetMap contributors. Visualisation: Ansgar Wolsing"
  ) + 
  theme_void(base_family = "Montserrat", base_size = 12) +
  theme(
    plot.background = element_rect(color = "grey8", fill = "grey8"),
    panel.background = element_rect(color = NA, fill = NA),
    text = element_text(color = "grey95"),
    plot.title = element_text(color = "#FEFEFE", family = "Chivo", 
                              size = 28, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    plot.margin = margin(rep(4, 4))
  ) 
ggsave(here("plots", "02-lines-rivers-de-with-shape.png"), dpi = 600, width = 7, height = 8)
