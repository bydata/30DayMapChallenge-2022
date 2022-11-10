library(tidyverse)
library(sf)
library(here)

#' https://www.destatis.de/DE/Service/EXDAT/Datensaetze/bevoelkerung-geo-mobilfunkdaten.html
#' https://www.destatis.de/static/DE/dokumente/Bevoelkerungsraster_1km_2020.zip
#' CRS: https://epsg.io/3035

raster <- readxl::read_xlsx(here("data", "Bevoelkerungsraster (1 km x 1 km).xlsx"), 
                  sheet = 2)


raster_prep <- raster %>% 
  transmute(lat = str_extract(`Gitter-ID`, "[SN]\\d{7}"),
            lat = str_remove(lat, "[SN]") %>% as.numeric(),
            lon = str_extract(`Gitter-ID`, "[EW]\\d{7}"),
            lon = str_remove(lon, "[EW]") %>% as.numeric(),
            Gemeinde,
            pop = ifelse(Exp_georef_BFS_20 == "[0-3]", 1.5, as.numeric(Exp_georef_BFS_20))) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:3035") %>% 
  st_transform(crs = "EPSG:4839")
st_crs(raster_prep)

p <- raster_prep %>% 
  ggplot() +
  geom_point(aes(x = st_coordinates(geometry)[, "X"], y = st_coordinates(geometry)[, "Y"],
                 col = pop, size = pop)) +
  scale_size_area(max_size = 16) +
  scale_color_distiller(direction = 1) +
  coord_sf() +
  guides(size = "none", col = "none") +
  labs(title = "Population Density in Germany") +
  theme_void(base_family = "Fuzzy Bubbles") +
  theme(
    plot.background = element_rect(color = "grey81", fill = "grey81"),
    plot.margin = margin(6, 6, 6, 6),
    plot.title = element_text(hjust = 0.5, size = 28, face = "bold")
  )
ggsave(here("plots", "10-bad-map.png"), dpi = 300, width = 7, height = 8)
