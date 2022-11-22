library(tidyverse)
library(sf)
library(ggtext)
library(here)

#' Source: Kontur
#' https://data.humdata.org/dataset/kontur-population-new-zealand

#' Font: 
# Fira Sans: https://www.govt.nz/about/about-this-website/typography/

kontur <- st_read(here("data", "kontur_population_NZ_20220630.gpkg"))
st_crs(kontur)
# https://www.linz.govt.nz/guidance/geodetic-system/coordinate-systems-used-new-zealand/projections/new-zealand-transverse-mercator-2000-nztm2000
# https://epsg.io/2193
kontur <- st_transform(kontur, crs = "EPSG:2193")
st_bbox(kontur)

# Functions for custom annotations
annotate_text <- function(hjust = 0,...) {
  annotate(
    "richtext",
    lineheight = 1.75, vjust = 0.5, color = "white", family = "Fira Sans", 
    hjust = hjust, fill = NA, label.size = 0, size = 4,
    ...)
}

annotate_segment <- function(...) {
  annotate(
    GeomSegment,
    ..., size = 0.5, color = "white"
  ) 
}


p <- ggplot(kontur) +
  geom_sf(aes(fill = population), size = 1e-3, color = "white") +
  annotate_text(
    label = "**Auckland**<br>pop. 1,675,000",
    x = 1106376, y = 5.9e6) +
  annotate_segment(x = 1120000, xend = 1.75e6, y = 5.9e6, yend = 5.9e6) +
  annotate_text(
    label = "**Christchurch**<br>pop. 389,300",
    x = 2.21e6, y = 5.1775e6, hjust = 1) +
  annotate_segment(x = 2.2e6, xend = 1.58e6, y = 5.1775e6, yend = 5.1775e6) +
  annotate_text(
    label = "**Wellington**<br>pop. 212,000",
    x = 2.292e6, y = 5.44e6, hjust = 1) +
  annotate_segment(x = 2.28e6, xend = 1.77e6, y = 5.44e6, yend = 5.44e6) +
  scale_fill_viridis_c(breaks = c(10, 100, 1000, 3000, 7000), 
                       labels = scales::number_format(),
                       trans = "pseudo_log", option = "plasma") +
  coord_sf(ylim = c(4765601, 6231366)) +  # c(4165601, 6731366)
  labs(
    title = "New Zealand Population Density",
    subtitle = "400m hexagon population grid. Values represent number of people 
    in cell. Release 2022-06-30.",
    caption = "Data: Kontur Population Dataset. Visualisation: Ansgar Wolsing",
    fill = "Population"
  ) +
  theme_void(base_family = "Fira Sans") +
  theme(
    plot.background = element_rect(color = "grey8", fill = "grey8"),
    text = element_text(color = "grey90"),
    plot.title = element_text(
      color = "white", family = "Fira Sans", face = "bold", hjust = 0.5, 
      size = 24),
    plot.subtitle = element_textbox(
      lineheight = 1.2, width = 1, hjust = 0.5, halign = 0.5),
    legend.position = c(0.1, 0.5),
    legend.direction = "vertical",
    legend.key.width = unit(3, "mm"),
    legend.text = element_text(size = 8),
    plot.margin = margin(c(t = 2, b = 0, l = 10, r = 10))
  )
ggsave(here("plots", "21-kontur-nz.png"), dpi = 600, width = 8, height = 8)
