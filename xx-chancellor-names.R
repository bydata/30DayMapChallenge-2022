pacman::p_load("tidyverse", "here", "glue", "ggtext", "colorspace", "scico",
               "sf", "osmdata", "gganimate")

## GEOMETRIES ==================================================================

crs <- "EPSG:4326"

## Shape Germany
de <- rnaturalearth::ne_countries(scale = 10, country = "Germany", returnclass = "sf")
st_crs(de)

ggplot(de) +
  geom_sf()

# https://wiki.openstreetmap.org/wiki/Map_features#Name

#' Which keys to retrieve?
#' https://help.openstreetmap.org/questions/64879/get-all-bicycle-infrastructure-for-a-city
#' all roads that have additional tags indicating cycle infrastructure
#'      (cycleway=lane and cycleway=track)
#' all highway=cycleway
#' all highway=footway and highway=path that allow bicycle use or are
#'      intended for such (bicycle=yes, bicycle=designated, bicycle=official)




## ------------------------------------------------

library(geojsonio)

chancellor_geoson_files <- here("data", "osm-chancellor-streets", c("erhard.geojson", "kohl.geojson"))

erhard_geojson <-  geojson_read(chancellor_geoson_files[1])
# geojson_list(erhard_geojson)

erhard <- st_read(chancellor_geoson_files[1])

erhard_birthplace <- getbb("Fürth, Bayern, Deutschland")
erhard_birthplace_cnt <- c(
  x = (erhard_birthplace["x", "min"] + erhard_birthplace["x", "max"]) / 2,
  y = (erhard_birthplace["y", "min"] + erhard_birthplace["y", "max"]) / 2
  )

ggplot(de) +
  geom_sf(color = "grey70", size = 0.3) + 
  annotate(
    "point",
    x = erhard_birthplace_cnt["x"], y = erhard_birthplace_cnt["y"],
    color = "grey4", shape = 21, fill = NA, stroke = 0.9, size = 3
  ) +
  annotate(
    "text",
    x = erhard_birthplace_cnt["x"], y = erhard_birthplace_cnt["y"],
    color = "grey4", label = "Fürth",
    hjust = -0.1, vjust = 2, family = "Oswald", size = 4
  ) +
  geom_sf(data = st_centroid(erhard), fill = "#59C3C3", size = 2.5,
          shape = 21, color = "white") +
  labs(
    title = "Ludwig Erhard",
    subtitle = "Chancellor 1963-1966"
  ) + 
  theme_void(base_family = "Helvetica Neue") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    text = element_text(color = "grey20"),
    plot.title = element_text(family = "Oswald", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )
ggsave(here("plots", "xx-chancellors-streets-erhard.png"), width = 6, height = 7)


kohl <- st_read(chancellor_geoson_files[2])
kohl_birthplace <- getbb("Ludwigshafen am Rhein, Deutschland")
kohl_birthplace_cnt <- c(
  x = (kohl_birthplace["x", "min"] + kohl_birthplace["x", "max"]) / 2,
  y = (kohl_birthplace["y", "min"] + kohl_birthplace["y", "max"]) / 2
)
ggplot(de) +
  geom_sf(color = "grey70", size = 0.3) + 
  annotate(
    "point",
    x = kohl_birthplace_cnt["x"], y = kohl_birthplace_cnt["y"],
    color = "grey4", shape = 21, fill = NA, stroke = 0.9
  ) +
  annotate(
    "text",
    x = kohl_birthplace_cnt["x"], y = kohl_birthplace_cnt["y"],
    color = "grey4", label = "Ludwigshafen",
    hjust = -0.1, vjust = -1, family = "Oswald", size = 4
  ) +
  geom_sf(data = st_centroid(kohl), fill = "#59C3C3", size = 2.5, 
        shape = 21, color = "white") +
  labs(
    title = "Helmut Kohl",
    subtitle = "Chancellor 1982-1998"
  ) + 
  theme_void(base_family = "Helvetica Neue") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    text = element_text(color = "grey20"),
    plot.title = element_text(family = "Oswald", hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = 0.5)
  )
ggsave(here("plots", "xx-chancellors-streets-kohl.png"), width = 6, height = 7)


## Turn it into a function

plot_chancellor_streets <- function(geojson_file, 
                                    birthplace_label = "",
                                    birthplace_fullname = "",
                                    chancellor_name = "",
                                    chancellor_period = "",
                                    bgshape = de) {
  
  streets <- st_read(geojson_file)
  streets_filtered <- st_filter(streets, bgshape)
  
  birthplace <- getbb(birthplace_fullname)
  birthplace_cnt <- c(
    x = (birthplace["x", "min"] + birthplace["x", "max"]) / 2,
    y = (birthplace["y", "min"] + birthplace["y", "max"]) / 2
  )
  
  ggplot(bgshape) +
    geom_sf(color = "grey70", size = 0.3) + 
    annotate(
      "point",
      x = birthplace_cnt["x"], y = birthplace_cnt["y"],
      color = "grey4", shape = 21, fill = NA, stroke = 0.9, size = 3
    ) +
    annotate(
      "text",
      x = birthplace_cnt["x"], y = birthplace_cnt["y"],
      color = "grey4", label = birthplace_label,
      hjust = -0.1, vjust = 2, family = "Oswald", size = 4
    ) +
    geom_sf(data = st_centroid(streets_filtered), fill = "#59C3C3", size = 2.5,
            shape = 21, color = "white") +
    labs(
      title = chancellor_name,
      subtitle = chancellor_period
    ) + 
    theme_void(base_family = "Helvetica Neue") +
    theme(
      plot.background = element_rect(color = "white", fill = "white"),
      text = element_text(color = "grey20"),
      plot.title = element_text(family = "Oswald", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

plot_chancellor_streets(here("data", "osm-chancellor-streets", "erhard.geojson"),
                        birthplace_label = "Fürth",
                        birthplace_fullname = "Fürth, Deutschland",
                        chancellor_name = "Ludwig Erhard",
                        chancellor_period = "1963 to 1966")

plot_chancellor_streets(here("data", "osm-chancellor-streets", "adenauer.geojson"),
                        birthplace_label = "Köln",
                        birthplace_fullname = "Köln, Deutschland",
                        chancellor_name = "Konrad Adenauer",
                        chancellor_period = "1949 to 1963")
ggsave(here("plots", "xx-chancellors-streets-adenauer.png"), width = 6, height = 7)
