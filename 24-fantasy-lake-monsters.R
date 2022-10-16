library(tidyverse)
library(sf)
library(ggmap)
library(here)
library(ggtext)


#' Read Lake Monster data
#' Source: 
#' https://en.wikipedia.org/wiki/List_of_lake_monsters
lake_monsters <- read_tsv(here("data", "lake-monsters.tsv"),
                          na = c("---", "#ERROR!"),
                          name_repair = janitor::make_clean_names)

lake_monsters %>% 
  count(lake_monster_type, sort = TRUE)

lake_monsters <- lake_monsters %>% 
  mutate(lake_monster_type_recoded = if_else(
           is.na(lake_monster_type) | lake_monster_type == "Other",
           "Other/No description",
           lake_monster_type
         ),
         lake_monster_type_recoded = fct_lump_min(
           lake_monster_type_recoded, min = 3, other_level = "Other/No description"))


lake_monsters_sf <- lake_monsters %>% 
  filter(!is.na(long), !is.na(lat)) %>% 
  st_as_sf(coords = c("long", "lat"), crs = "EPSG:4236") %>% 
  st_transform(crs = "ESRI:54009")

# A map of the world
world <- map_data("world")

# ggplot() +
#   geom_map(
#     data = world,
#     map = world,
#     aes(long, lat, map_id = region)) +
#   coord_map(projection = "mollweide", xlim = c(-179.9, 179.9))

ggplot() +
  geom_map(
    data = world,
    map = world,
    aes(long, lat, map_id = region), fill = "grey73") +
  # geom_point(
  #   data = lake_monsters_sf,
  #   aes(x = st_coordinates(geometry)[, "X"], y = st_coordinates(geometry)[, "Y"]),
  #   color = "red"
  # ) +
  geom_point(
    data = lake_monsters,
    aes(long, lat, fill = lake_monster_type_recoded),
    shape = 21, color = "white", stroke = 0.1, size = 2.5) +
  annotate(
    "text",
    x = -35, y = 47, label = "Here's Nessie!", color = "grey92", 
    family = "Playfair Display", size = 6, vjust = 1,
  ) +
  #  57.3228575, -4.4243817
  annotate(
    "segment",
    x = -20, xend = -7, y = 53, yend = 57.5, color = "grey92", size = 0.25
  ) +
  scale_fill_manual(values = c(MetBrewer::met.brewer("Juarez"), "grey50")) +
  coord_map(projection = "orthographic", xlim = c(-179.9, 179.9),
            orientation = c(60, -10, 45)) +
  guides(fill = guide_legend(title.position = "top", override.aes = list(size = 4), 
                             direction = "vertical")) +
  labs(
    title = "Lake Monster Sightings Across the World",
    subtitle = "A lake monster is a lake-dwelling entity in folklore.
    The most famous example is the Loch Ness Monster (Nessie).<br>
    Monster types derived from Wikipedia descriptions.",
    caption = "Source: English Wikipedia. Visualisation: Ansgar Wolsing",
    fill = "Monster type"
  ) +
  theme_void(base_family = "Playfair Display") +
  theme(
    plot.background = element_rect(color = "grey2", fill = "grey2"),
    plot.margin = margin(l = 6, t = 2, b = 2, r = 6),
    legend.position = c(0.01, 0.2),
    text = element_text(color = "grey97"),
    legend.justification = "left",
    legend.background = element_rect(color = "grey60", fill = alpha("grey60", 0.5),
                                     size = 0.2),
    legend.margin = margin(5, 5, 5, 5),
    plot.title = element_markdown(size = 24, family = "Playfair Display", 
                                  face = "italic", hjust = 0.5),
    plot.subtitle = element_textbox(
      width = 0.9, lineheight = 1.2, hjust = 0.5, halign = 0.5, margin = margin(t = 4, b = 12)),
    plot.caption = element_markdown(hjust = 0.5)
  )
ggsave(here("plots", "24-fantasy-lake-monsters.png"), dpi = 600, width = 7, height = 7)
