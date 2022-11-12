library(tidyverse)
library(sf)
library(osmdata)
library(ggtext)
library(here)


## GET DATA ====================================================================
#' Source: https://www.offenedaten-koeln.de/dataset/adresse
#' Source: https://www.offenedaten-koeln.de/dataset/geb%C3%A4udemodell-stadt-k%C3%B6ln-2010
#'    Projection: 31466 - DHDN / Gauss-Kruger zone 2. VG

urls_buildings <- paste0("https://www.offenedaten-koeln.de/sites/default/files/dachansicht_lod2_part",
                         1:3, ".zip")
filepaths_buildings_zip <- here("data", "cologne_buildings",
                                paste0("dachansicht_lod2_part", 1:3, ".zip"))
folder_buildings <- here("data", "cologne_buildings")
filepath_buildings_dataframe <- here(folder_buildings, "cologne_buildings.rds")

if (!file.exists(filepath_buildings_dataframe)) {
  if (!file.exists(filepaths_buildings_zip[1])) {
    dir.create(folder_buildings)
    walk2(urls_buildings, filepaths_buildings_zip,
          ~download.file(url = .x, destfile = .y, mode = "wb"))
    walk(filepaths_buildings_zip, unzip, exdir =  folder_buildings)
  }
  
  filepaths_shp <- here(folder_buildings,
                        list.files(folder_buildings, pattern = ".*\\.shp$"))
  buildings <- map_dfr(filepaths_shp, st_read)
  
  # Save buildings dataframe with geometry
  write_rds(buildings, filepath_buildings_dataframe,
            compress = "gz")
} else {
  buildings <- read_rds(filepath_buildings_dataframe)
}

#' Set the coordinate reference system
#' According to comments/documentation: 31466 - DHDN / Gauss-Kruger zone 2. VG
st_crs(buildings$geometry) <- "EPSG:31466"
st_crs(buildings$geometry)

View(head(buildings$geometry, 1))
buildings %>% 
  st_drop_geometry() %>% 
  arrange(-First_H) %>% 
  select(First_H, First_H_NN, NUTZUNG) %>% head(10)
buildings %>% 
  st_drop_geometry() %>% 
  arrange(Trauf_H) %>% 
  select(Trauf_H, First_H, First_H_NN, NUTZUNG) %>% head(10)

buildings2 <- st_zm(buildings, drop = TRUE, what = "ZM")

# Inspiration: https://twitter.com/dr_xeo/status/1392794246009311236

# Coordinates of the Cologne Cathedral: 50.9412784,6.9560927
# cathedral_point <- st_point(c(6.9560927, 50.9412784))
cathedral_point <- st_as_sfc("POINT(6.9560927 50.9412784)", crs = "EPSG:4326") %>% 
  st_as_sf() %>% 
  st_transform(crs = "EPSG:31466")

cathedral_point_buffered <- cathedral_point %>%
  st_buffer(2500) 

buildings2_around_cathedral <- buildings2 %>% 
  st_filter(cathedral_point_buffered)

p <- buildings2 %>% 
  ggplot() +
  geom_sf(aes(fill = First_H > 20), color = "grey12", size = 1e-4) +
  coord_sf() +
  scale_fill_manual(values = c("FALSE" = "grey64", "TRUE" = "#EE4266")) +
  # paletteer::scale_fill_paletteer_d("colorBlindness::Blue2DarkRed12Steps") +
  # guides(fill = guide_legend(title.position = "top", direction = "horizontal", byrow = FALSE)) +
  labs(
    title = "COLOGNE",
    caption = "Source: Offene Daten Köln. 
    Visualisation: Ansgar Wolsing (Inspiration by @dr_xeo)",
    fill = "Height in meters"
  ) +
  theme_void(base_family = "Cabinet Grotesk") +
  theme(
    plot.background = element_rect(color = "grey90", fill = "grey90"),
    plot.margin = margin(rep(10, 4)),
    text = element_text(color = "grey99"),
    plot.title = element_text(size = 36, face = "bold", hjust = 0.5, color = "white"),
    plot.caption = element_markdown(hjust = 0.5),
    legend.position = "bottom"
  )
ggsave(here("plots", "12-scale-buildings-cgn-height.png"), dpi = 300, width = 10, height = 10)
