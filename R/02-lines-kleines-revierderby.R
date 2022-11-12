library(tidyverse)
library(sf)
library(googleway)
library(ggmap)
library(here)
library(ggtext)

# Authenticate Google Maps API 
# Get API key: https://developers.google.com/maps/documentation/embed/get-api-key?hl=en
# set key value pair via R console
api_key <- Sys.getenv("gmaps_api_key") 
set_key(key = api_key, api = "directions")
set_key(key = api_key, api = "distance")
google_keys()

start_addr <- "Westfalenstadion, Dortmund"
destination_addr <- "Ruhrstadion, Bochum"

# Directions
df <- google_directions(origin = start_addr,
                        destination = destination_addr,
                        key = api_key,
                        mode = "driving",
                        simplify = TRUE)
pl <- direction_polyline(df)
pl_decoded <- decode_pl(pl)

# Distance
distance <- google_distance(origin = start_addr,
                  destination = destination_addr,
                  key = api_key,
                  mode = "driving",
                  simplify = TRUE)


min(pl_decoded$lon)
max(pl_decoded$lon)
min(pl_decoded$lat)
max(pl_decoded$lat)

pl_decoded %>% 
  ggplot() +
  geom_line(aes(lon, lat), color = "red")

buffer_for_bb <- 0.3
pl_bbox <- c(left = min(pl_decoded$lon) - buffer_for_bb,
             bottom = min(pl_decoded$lat) - buffer_for_bb / 2, 
             right = max(pl_decoded$lon) + buffer_for_bb,
             top = max(pl_decoded$lat) + buffer_for_bb / 2)

start_point <- pl_decoded[1, ]
end_point <- pl_decoded[nrow(pl_decoded), ]

# map_tonerhybrid <- get_stamenmap(bbox = pl_bbox, zoom = 10, maptype = "toner-hybrid")
map_toner <- get_stamenmap(bbox = pl_bbox, zoom = 10, maptype = "toner")

subtitle = sprintf(
  "**'Kleines' Revierderby** refers to the association football matches between 
  Borussia Dortmund and VfL Bochum. <br><br>
  There have been 79 competitive matches between 1960 and 2022.
  Dortmund have won 31 matches, Bochum 17, with 21 draws  (as of 03 November 2022).<br><br>
  The stadiums are located only **%s** away.",
  distance$rows$elements[[1]]$distance$text
)

ragg::agg_png(here("plots", "02-lines-directions-bvb-boc.png"), res = 500, 
              units = "in", width = 7, height = 5)
ggmap(map_toner, darken = c(0.1, "grey10"), extent = "device") +
  geom_line(
    data = pl_decoded, aes(lon, lat), color = "grey20", size = 1.5) +
  geom_line(
    data = pl_decoded, aes(lon, lat), color = "#FDE100", size = 1.2) +
  annotate(
    "richtext",
    label = sprintf(
      "<img src='%s' width='35'>", 
      here("input", "team_icons", "Borussia Dortmund.png")),
    x = start_point$lon, y = start_point$lat, size = 2,
    fill = NA, label.size = 0) +
  annotate(
    "richtext",
    label = sprintf(
      "<img src='%s' width='35'>", 
      here("input", "team_icons", "VfL Bochum.png")),
    x = end_point$lon, y = end_point$lat, size = 2,
    fill = NA, label.size = 0) +
  annotate(
    GeomTextBox,
    label = subtitle,
    x = pl_bbox["left"], y = pl_bbox["bottom"], hjust = -0.05, vjust = -0.1,
    fill = alpha("white", 0.95), box.size = 0.1, box.r = unit(1, "mm"),
    family = "Source Sans Pro", width = 0.5, size = 3
  ) +
  labs(
    title = "Road to #BVBBOC",
    caption = "Source: Wikipedia. Map tiles: Stamen Design.
    Visualisation: Ansgar Wolsing"
  ) +
  theme_void(base_family = "Source Sans Pro") +
  theme(
    plot.background = element_rect(color = "white", fill = "white"),
    plot.title = element_text(family = "Bebas Neue", hjust = 0.5, size = 32),
    plot.caption = element_markdown(margin = margin(t = 4, l = 2, r = 2))
  )
invisible(dev.off())
