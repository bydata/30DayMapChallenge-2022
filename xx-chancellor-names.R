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

#' HOW-TO
#' All OSM data was extracted via Overpass Turbo API on https://overpass-turbo.eu/
#' The queries can be found in the data folder

library(geojsonio)

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
    geom_sf(data = st_centroid(streets_filtered), fill = "#59C3C3", size = 2.5,
            shape = 21, color = "white") +
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
    labs(
      title = chancellor_name,
      subtitle = chancellor_period
    ) + 
    theme_void(base_family = "Helvetica Neue") +
    theme(
      plot.background = element_rect(color = "white", fill = "white"),
      text = element_text(color = "grey20"),
      plot.title = element_text(family = "Oswald", color = "grey2", hjust = 0.5),
      plot.subtitle = element_markdown(hjust = 0.5, lineheight = 1.25)
    )
}


p1 <- plot_chancellor_streets(here("data", "osm-chancellor-streets", "adenauer.geojson"),
                        birthplace_label = "Köln",
                        birthplace_fullname = "Köln, Deutschland",
                        chancellor_name = "Konrad Adenauer",
                        chancellor_period = "Chancellor from 1949 to 1963<br>*1876 \U2020 1967")
ggsave(here("plots", "xx-chancellors-streets-adenauer.png"), width = 6, height = 7)


p2 <- plot_chancellor_streets(here("data", "osm-chancellor-streets", "erhard.geojson"),
                        birthplace_label = "Fürth",
                        birthplace_fullname = "Fürth, Deutschland",
                        chancellor_name = "Ludwig Erhard",
                        chancellor_period = "Chancellor from 1963 to 1966<br>*1897 \U2020 1977")
ggsave(here("plots", "xx-chancellors-streets-erhard.png"), width = 6, height = 7)


p3 <- plot_chancellor_streets(here("data", "osm-chancellor-streets", "kiesinger.geojson"),
                        birthplace_label = "Ebingen",
                        birthplace_fullname = "Ebingen, Deutschland",
                        chancellor_name = "Kurt Georg Kiesinger",
                        chancellor_period = "Chancellor from 1966 to 1969<br>*1904 \U2020 1988")
ggsave(here("plots", "xx-chancellors-streets-kiesinger.png"), width = 6, height = 7)


p4 <- plot_chancellor_streets(here("data", "osm-chancellor-streets", "brandt.geojson"),
                        birthplace_label = "Lübeck",
                        birthplace_fullname = "Lübeck, Deutschland",
                        chancellor_name = "Willy Brandt",
                        chancellor_period = "Chancellor from 1969 to 1972<br>*1913 \U2020 1992")
ggsave(here("plots", "xx-chancellors-streets-brandt.png"), width = 6, height = 7)


p5 <- plot_chancellor_streets(here("data", "osm-chancellor-streets", "schmidt.geojson"),
                        birthplace_label = "Hamburg",
                        birthplace_fullname = "Hamburg, Deutschland",
                        chancellor_name = "Helmut Schmidt",
                        chancellor_period = "Chancellor from 1972 to 1982<br>*1918 \U2020 2015")
ggsave(here("plots", "xx-chancellors-streets-schmidt.png"), width = 6, height = 7)


p6 <- plot_chancellor_streets(here("data", "osm-chancellor-streets", "kohl.geojson"),
                        birthplace_label = "Ludwigshafen",
                        birthplace_fullname = "Ludwigshafen am Rhein, Deutschland",
                        chancellor_name = "Helmut Kohl",
                        chancellor_period = "Chancellor from 1982 to 1998<br>*1930 \U2020 2017")
ggsave(here("plots", "xx-chancellors-streets-kohl.png"), width = 6, height = 7)


p7 <- plot_chancellor_streets(here("data", "osm-chancellor-streets", "schroeder.geojson"),
                        birthplace_label = "Mossenberg",
                        birthplace_fullname = "Mossenberg, Deutschland",
                        chancellor_name = "Gerhard Schröder",
                        chancellor_period = "Chancellor from 1998 to 2005<br>*1994")
ggsave(here("plots", "xx-chancellors-streets-schroeder.png"), width = 6, height = 7)


library(patchwork)

(p1 + p2 + p3) /
  (p4 + p5 + p6) +
  plot_annotation(
    title = "Where the Streets Have German Chancellor's Names",
    subtitle = "Streets which are named after chancellors of the Federal Republic of Germany",
    caption = "Source: OpenStreetMap contributors. Visualisation: Ansgar Wolsing",
    theme = theme(
      text = element_text(family = "Helvetica Neue", color = "grey28"),
      plot.title = element_text(hjust = 0.5, size = 24, color = "grey8",
                                family = "Oswald"),
      plot.subtitle = element_text(hjust = 0.5)
    )
  )
  
ggsave(here("plots", "xx-chancellors-streets-combined.png"), width = 10, height = 9)
