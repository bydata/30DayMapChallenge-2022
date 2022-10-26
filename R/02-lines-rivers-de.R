library(tidyverse)
library(sf)
library(osmdata)
library(here)

de <- rnaturalearth::ne_countries(scale = 10, country = "Germany", returnclass = "sf")

rivers <- opq(bbox = st_bbox(de), timeout = 1800) %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

write_rds(rivers, here("data", "osm-rivers.rds"), compress = "gz")

rivers_lines_filtered <- st_filter(rivers$osm_lines, de)
# rivers_multilines_filtered <- st_intersection(rivers$osm_multilines, de)
rivers_multilines_filtered <- st_intersection(rivers$osm_multilines, 
                                              st_buffer(de, 1))

# rivers_lines_filtered %>% 
#   select(osm_id, name, width, geometry) %>% 
#   mutate(length = st_length(geometry)) %>% 
#   arrange(-length)
rivers_multilines_filtered_prep <- rivers_multilines_filtered %>% 
  select(osm_id, name, name.en, width, geometry) %>% 
  mutate(length = st_length(geometry),
         length_km = as.numeric(length) / 1000,
         name_consolidated = ifelse(!is.na(name.en) & name.en != "", name.en, name)) %>% 
  arrange(-length)

rivers_multilines_filtered_prep %>% 
  st_drop_geometry() %>% 
  head(10)


p <- rivers_multilines_filtered_prep %>% 
  ggplot(aes(color = length_km)) +
  geom_sf(
    data = de,
    aes(geometry = geometry), fill = "grey16", color = "grey70", size = 0.01) +
  geom_sf(aes(size = length_km)) +
  geom_sf_label(
    data = ~slice_max(rivers_multilines_filtered_prep, order_by = length, n = 10),
    aes(label = name_consolidated)) +
  scico::scale_color_scico(palette = "nuuk") + #bamako #acton #nuuk
  scale_size_continuous(range = c(0.1, 0.8)) +
  guides(size = "none",
         color = "none") +
  theme_void() +
  theme(
    plot.background = element_rect(color = "grey8", fill = "grey8"),
    panel.background = element_rect(color = NA, fill = NA)
  )
ggsave(here("plots", "02-lines-rivers-de.png"), dpi = 600, width = 7, height = 8)


rivers$osm_lines %>% 
  filter(name == "Oder") %>% 
  select(osm_id, name, name.en, width, geometry) %>% 
  mutate(length = st_length(geometry),
         length_km = as.numeric(length) / 1000,
         name_consolidated = ifelse(!is.na(name.en) & name.en != "", name.en, name)) %>% 
  arrange(-length) %>% 
  summarize(sum(length))
  
