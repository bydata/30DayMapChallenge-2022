library(tidyverse)
library(ggtext)
library(here)
library(jsonlite)
library(sf)
library(patchwork)

#' Source: https://www.flightsfrom.com/CGN
flights_cgn_json_url <- "https://www.flightsfrom.com/api/airport/CGN?from=CGN"
flights_cgn <- fromJSON(flights_cgn_json_url)
str(flights_cgn)

colnames(flights_cgn$response$routes)
routes_cgn <- flights_cgn$response$routes %>% 
  tibble() %>% 
  unnest(cols = c(airport)) %>% 
  select(iata_from, iata_to, city_name_en, flights_per_week, latitude, longitude, 
         starts_with("country")) %>% 
  mutate(across(c(latitude, longitude, flights_per_week), as.numeric))

cgn_latitude <- as.numeric(flights_cgn$response$airport$latitude)
cgn_longitude <- as.numeric(flights_cgn$response$airport$longitude)

# Check if there are any cities with multiple airports among the destinations
count(routes_cgn, city_name_en, sort = TRUE) %>% 
  filter(n > 1) # >> London (3), Istanbul (2)

# Aggregate destinations by city name
routes_cgn_agg <- routes_cgn %>% 
  group_by(iata_from, city_name_en, country_code, country) %>% 
  summarize(
    flights_per_week = sum(flights_per_week),
    latitude = mean(latitude), longitude = mean(longitude),
    .groups = "drop"
  ) %>% 
  mutate(flights_per_week = replace_na(flights_per_week, 1))
  

# Destination countries for map background
destination_countries <- unique(routes_cgn$country)
destination_countries[order(destination_countries)]
destination_countries[destination_countries == "Czech Republic"] <- "Czechia"

destination_country_shapes <- rnaturalearth::ne_countries(
  scale = 50, country = destination_countries, returnclass = "sf")

europe <- rnaturalearth::ne_countries(scale = 50, continent = "Europe", returnclass = "sf")
africa <- rnaturalearth::ne_countries(scale = 50, continent = "Africa", returnclass = "sf")
asia <- rnaturalearth::ne_countries(scale = 50, continent = "Asia", returnclass = "sf")
st_crs(europe)


## Map in dark mode ============================================================

# constant breaks for the plots
size_breaks <- c(5, 10, 20, 40, 60)

p1 <- routes_cgn_agg %>% 
  ggplot() +
  geom_sf(
    data = europe,
    size = 0.2, fill = "grey20"
  ) + 
  geom_sf(
    data = asia,
    size = 0.2, fill = "grey20"
  ) + 
  geom_sf(
    data = africa,
    size = 0.2, fill = "grey20"
  ) +
  ggfx::with_shadow(
    geom_sf(
      data = destination_country_shapes,
      size = 0.2, fill = "grey30"
    ) 
  ) +
  ggfx::with_outer_glow(
    geom_curve(
      aes(x = cgn_longitude, y = cgn_latitude, xend = longitude, yend = latitude,
          # size = flights_per_week, 
          alpha = flights_per_week),
      size = 0.5,
      curvature = 0.3, color = "green"),
    color = "white", expand = 3, sigma = 5
  ) +
  annotate("point", x = cgn_longitude, y = cgn_latitude, 
           size = 3, shape = 21, stroke = 1, fill = "grey90") +
  geom_point(
    aes(longitude, latitude, size = flights_per_week),
    color = "grey82"
  ) +
  # Label most frequent airports (outside Germany)
  ggrepel::geom_label_repel(
    data = ~filter(., country != "Germany") %>% 
      slice_max(order_by = flights_per_week, n = 3),
    aes(x = longitude, latitude, label = city_name_en),
    family = "Roboto Condensed", size = 2.5, 
    fill = alpha("white", 0.6), label.size = 0.1
  ) + 
  scale_size(range = c(0.1, 5), breaks = size_breaks,
             limits = c(0, max(routes_cgn$flights_per_week))) +
  scale_alpha_continuous(range = c(0.3, 1), breaks = size_breaks) +
  coord_sf(xlim = c(-20, 58), ylim = c(25, 68)) +
  theme_void(base_family = "Roboto Condensed") +
  theme(
    legend.position = "bottom",
    plot.background = element_rect(color = "grey8", fill = "grey8"),
    text = element_text(color = "grey90"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5,
                              margin = margin(t = 4, b = 8)),
    plot.caption = element_markdown(lineheight = 1.1, hjust = 0)
  )
p1

# Within Germany
p2 <- routes_cgn %>% 
  filter(country == "Germany") %>% 
  ggplot() +
  ggfx::with_shadow(
    geom_sf(
      data = subset(europe, sovereignt == "Germany"),
      size = 0.2, fill = "grey30"
    ) 
  ) +
  ggfx::with_outer_glow(
    geom_curve(
      aes(x = cgn_longitude, y = cgn_latitude, xend = longitude, yend = latitude,
          # size = flights_per_week, 
          alpha = flights_per_week),
      size = 0.5,
      curvature = 0.2, color = "green"),
    color = "white", expand = 3, sigma = 5
  ) +
  annotate("point", x = cgn_longitude, y = cgn_latitude, 
           size = 3, shape = 21, stroke = 1, fill = "grey90") +
  geom_point(
    aes(longitude, latitude, size = flights_per_week),
    color = "grey82"
  ) +
  geom_text(
    aes(x = longitude, y = latitude, label = city_name_en),
    family = "Roboto Condensed", hjust = -0.2, color = "grey90"
  ) +
  scale_size(range = c(0.1, 5), breaks = size_breaks,
             limits = c(0, max(routes_cgn$flights_per_week))) +
  scale_alpha_continuous(range = c(0.3, 1), breaks = size_breaks) +
  coord_sf() +
  theme_void(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "grey8", fill = "grey8"),
    panel.background = element_rect(color = "grey39", fill = "grey8"),
    text = element_text(color = "grey90"),
    legend.position = "none"
  )

p1 + 
  labs(
    title = toupper("Destinations from Cologne-Bonn Airport"),
    caption = "There are flights from Cologne-Bonn to 3 airports in London and 
    2 airports in Istanbul. Their numbers have been aggregated.
    <br>
    Source: flightsfrom.com. Visualisation: Ansgar Wolsing",
    size = "Flights per week", alpha = "Flights per week"
  ) +
  inset_element(p2, left = 0.6, top = 1, bottom = 0.4, right = 1.1) &
  theme(
    plot.background = element_rect(color = "grey8", fill = "grey8"),
    plot.caption = element_markdown(lineheight = 1.2)
  )
ggsave(here("plots", "06-network-flight-destinations-cgn+de-inset.png"), width = 7, height = 6)

