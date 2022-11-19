library(tidyverse)
library(here)
library(ggtext)
library(osmdata)
library(sf)

de <- rnaturalearth::ne_countries(scale = 50, country = "Germany", returnclass = "sf")
bb <- st_bbox(de)
bb

autobahn <- opq(bb, timeout = 1200) %>% 
  add_osm_feature(key = "highway", value = "motorway") %>% 
  osmdata_sf()
write_rds(autobahn, here("output", "osm_autobahn.rds"), compress = "gz")

autobahn_de <- st_filter(autobahn$osm_lines, de)

autobahn_color <- "#003298"

autobahn_de %>% 
  ggplot() +
  geom_sf(color = "grey10", size = 0.3) +
  theme_void()

autobahn_de %>% 
  ggplot() +
  geom_sf(color = "deeppink", size = 0.3) +
  theme_void() +
  theme(
    plot.background = element_rect(color = "grey2", fill = "grey2")
  )


autobahn_de %>% 
  ggplot() +
  geom_sf(color = autobahn_color, size = 0.3) +
  labs(
    title = "AUTOBAHN",
  ) +
  theme_void(base_family = "Familjen Grotesk") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16)
  )

autobahn_de %>% 
  ggplot() +
  geom_sf(color = "white", size = 0.2) +
  labs(
    title = "AUTOBAHN",
    caption = "**Source:** OpenStreetMap contributors. **Visualisation:** Ansgar Wolsing"
  ) +
  theme_void(base_family = "Cabinet Grotesk") +
  theme(
    plot.background = element_rect(color = autobahn_color, fill = autobahn_color),
    text = element_text(color = "white"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 32),
    plot.caption = element_markdown(hjust = 0.5)
  )
ggsave(here("plots", "16-minimal-autobahn-blue.png"), dpi = 600, width = 4, height = 6)

autobahn_de %>% 
  ggplot() +
  geom_sf(color = autobahn_color, size = 0.2) +
  labs(
    title = "AUTOBAHN",
    caption = "**Source:** OpenStreetMap contributors. **Visualisation:** Ansgar Wolsing"
  ) +
  theme_void(base_family = "Cabinet Grotesk") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    text = element_text(color = autobahn_color),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 32),
    plot.caption = element_markdown(hjust = 0.5)
  )
ggsave(here("plots", "16-minimal-autobahn-white.png"), dpi = 600, width = 4, height = 6)

