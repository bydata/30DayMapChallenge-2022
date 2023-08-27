#' INSPIRATION & code for Voronoi tesselation: 
#' Colin Angus
#' https://github.com/VictimOfMaths/30DayMapChallenge/blob/main/Day3_Polygons.R

library(tidyverse)
library(sf)
library(here)
library(osmdata)
library(ggtext)

df <- read_csv(here("data", "query-wikidata-stadiums.csv"))

bundesliga_clubs <- c(
  "Borussia Dortmund",
  "FC Bayern Munich",
  "VfB Stuttgart",
  "SC Freiburg",
  "SV Werder Bremen",
  "Bayer 04 Leverkusen",
  # "FC Schalke 04",
  "FC Augsburg",
  "1. FC Köln",
  "Borussia Mönchengladbach",
  "TSG 1899 Hoffenheim",
  "RB Leipzig",
  # "Hertha BSC",
  "1. FC Union Berlin",
  "VfL Wolfsburg",
  "1. FSV Mainz 05",
  "VfL Bochum",
  "Eintracht Frankfurt",
 "1. FC Heidenheim",
 "SV Darmstadt 98"
)

bundesliga_club_colors <- c(
  "#FDE100",
  "#DC052D",
  rgb(227, 34, 25, maxColorValue = 255),
  "black",
  "#1D9053",
  "#E32221",
  # "#004D9D",
  "#BA3733",
  "#ED1C24", 
  "#000000",
  "#1C63B7",
  "#dadada",
  # "#005CA9",
  "#D4011D",
  "#65B32E",
  "#ED1C24",
  "#005CA9",
  "#E1000F",
  "#E30614",
  "#004E9E"
)
names(bundesliga_club_colors) <- bundesliga_clubs

df_buli <- subset(df, clubLabel %in% bundesliga_clubs) %>% 
  group_by(clubLabel) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>%
  st_as_sf(wkt = "coordinates", crs = "EPSG:4326") %>% 
  st_transform(crs = "EPSG:4839")

## GEOMETRIES ==================================================================
## Shapefile Germany
de <- rnaturalearth::ne_countries(scale = 50, country = "Germany", returnclass = "sf")
de <- st_transform(de, crs = "EPSG:4839")

st_crs(de) == st_crs(df_buli)

# club icons
icons_folder <- here("input", "team_icons_2023-24")
icons_files <- list.files(icons_folder, pattern = ".png")
icons_tags <- glue::glue("<img src='{here(icons_folder, icons_files)}' height=30>")
names(icons_tags) <- bundesliga_clubs[order(bundesliga_clubs)]


## VORONOI TESSELATION =========================================================

# Create Voronoi cells based on club grounds
voronoi <- df_buli %>%
  st_union() %>%
  st_voronoi() %>%
  st_collection_extract()

# # intersect with Cologne shape
voronoi <- voronoi[unlist(st_intersects(de, voronoi))] %>%
  st_intersection(de) %>% 
  st_as_sf()

voronoi_buli <- st_join(voronoi, df_buli) %>% 
  inner_join(data.frame(clubLabel = bundesliga_clubs, primary_color = bundesliga_club_colors)) %>% 
  inner_join(data.frame(clubLabel = names(icons_tags) , icon = icons_tags)) %>%
  st_as_sf()


## PLOT ========================================================================

ragg::agg_png(here("plots", "03-polygons-bundesliga-voronoi-map-2023-24.png"), 
              width = 7.5, height = 9, res = 600, units = "in", bg = "grey94")
seed <- 4711
set.seed(seed)
voronoi_buli %>%
  ggplot() +
  geom_sf(
    aes(fill = primary_color),
    col = "white", size = 0.75, show.legend = FALSE) +
  geom_sf(data = df_buli,
          aes(geometry = coordinates), size = 3,
          shape = 21, col = "white", fill = "grey12") +
  geom_richtext(
    aes(
      label = icon,
        x = st_coordinates(st_centroid(geometry))[, "X"],
        y = st_coordinates(st_centroid(geometry))[, "Y"]),
    fill = NA, label.size = 0
  ) +
  scale_fill_identity() +
  labs(
    title = "Wenn alle den Bundesligisten<br>in ihrer Nähe unterstützen würden",
    subtitle = "Jeder Punkt innerhalb einer Fläche ist dem Stadion des jeweiligen 
    Vereins<br>näher als jedem anderen Stadion gelegen.",
    caption = "Shapefile: Natural Earth, Koordinaten der Stadien: Wikidata.<br> 
    Visualisierung: Ansgar Wolsing (adaptiert von @VictimOfMaths)."
  ) +
  theme_void(base_family = "Helvetica Neue") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey94"),
    plot.margin = margin(6, 6, 6, 6),
    plot.title = element_markdown(
      family = "Helvetica Neue", face = "bold", size = 24, angle = 1.5, 
      hjust = 0.5, lineheight = 1, margin = margin(t = 8, b = -6)),
    plot.subtitle = element_markdown(
      family = "Helvetica Neue", size = 12, angle = 1.5, hjust = 0.5, lineheight = 1,
      margin = margin(t = 8)),
    plot.caption = element_markdown(hjust = 0.5, lineheight = 1)
  )
dev.off()


# Calculate areas per club

voronoi_buli %>% 
  transmute(clubLabel, 
            area = st_area(geometry),
            area_km2 = as.numeric(area) / 1000^2,
            area_share = area_km2 / sum(area_km2)) %>% 
  st_drop_geometry() %>% 
  arrange(desc(area)) 
