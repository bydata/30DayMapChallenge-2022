#' INSPIRATION & code for Voronoi tesselation: 
#' Colin Angus
#' https://github.com/VictimOfMaths/30DayMapChallenge/blob/main/Day3_Polygons.R

library(tidyverse)
library(sf)
library(here)
library(osmdata)
library(ggtext)

# sparql_endpoint <- "https://query.wikidata.org/sparql?query=SELECT%20%3Fclub%20%3FclubLabel%20%3Fvenue%20%3FvenueLabel%20%3Fcoordinates%0AWHERE%0A%7B%0A%09%3Fclub%20wdt%3AP31%20wd%3AQ476028%20.%0A%09%3Fclub%20wdt%3AP115%20%3Fvenue%20.%0A%09%3Fvenue%20wdt%3AP625%20%3Fcoordinates%20.%0A%09SERVICE%20wikibase%3Alabel%20%7B%20bd%3AserviceParam%20wikibase%3Alanguage%20%22en%22%20%7D%0A%7D"
df <- read_csv(here("data", "query-wikidata-stadiums.csv"))

bundesliga_clubs <- c(
  "Borussia Dortmund",
  "FC Bayern Munich",
  "VfB Stuttgart",
  "SC Freiburg",
  "SV Werder Bremen",
  "Bayer 04 Leverkusen",
  "FC Schalke 04",
  "FC Augsburg",
  "1. FC Köln",
  "Borussia Mönchengladbach",
  "TSG 1899 Hoffenheim",
  "RB Leipzig",
  "Hertha BSC",
  "1. FC Union Berlin",
  "VfL Wolfsburg",
  "1. FSV Mainz 05",
  "VfL Bochum",
  "Eintracht Frankfurt",
 "SSV Jahn Regensburg",
 "1. FC Kaiserslautern",
 "SC Paderborn 07",
 "1. FC Heidenheim",
 "Hamburger SV", #x --> venueLabel Volksparkstadion
 "Fortuna Düsseldorf",
 "SV Sandhausen",
 "F.C. Hansa Rostock",
 "SV Darmstadt 98",
 "Holstein Kiel", #x --> 
 "1. FC Nürnberg",
 "FC St. Pauli",
 "1. FC Magdeburg",
 "SpVgg Greuther Fürth",
 "Hannover 96",
 "Karlsruher SC",
 "Arminia Bielefeld",
 "Eintracht Braunschweig"
)

bundesliga_club_colors <- c(
  "#FDE100",
  "#DC052D",
  rgb(227, 34, 25, maxColorValue = 255),
  "black",
  "#1D9053",
  "#E32221",
  "#004D9D",
  "#BA3733",
  "#ED1C24", 
  "#000000",
  "#1C63B7",
  "#dadada",
  "#005CA9",
  "#D4011D",
  "#65B32E",
  "#ED1C24",
  "#005CA9",
  "#E1000F",
  "#E30614",
  "#E2001A",
  "#045CA9",
  "#E2001A",
  "#0060AF",
  "#FF0000",
  "#000000",
  "#E53616",
  "#004E9E",
  "#045A9E",
  "#AA122C",
  "#93755E",
  "#036AB2",
  "#2EA641",
  "#169D33",
  "#013798",
  "#034E95",
  "#F5E33D"
)
names(bundesliga_club_colors) <- bundesliga_clubs

df_buli <- subset(df, clubLabel %in% bundesliga_clubs) %>% 
  group_by(clubLabel) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>%
  add_row(
    data.frame(club = NA, clubLabel = "Holstein Kiel", venue = NA,
               venueLabel = "Holstein-Stadion", coordinates = "Point(10.122171 54.348766)")
  ) %>% 
  add_row(subset(df, venueLabel == "Volksparkstadion")) %>% 
  mutate(clubLabel = ifelse(venueLabel == "Volksparkstadion", "Hamburger SV", clubLabel),
         club = ifelse(venueLabel == "Volksparkstadion", NA, club)) %>% 
  st_as_sf(wkt = "coordinates", crs = "EPSG:4326") %>% 
  st_transform(crs = "EPSG:4839")

## GEOMETRIES ==================================================================
## Shapefile Germany
de <- rnaturalearth::ne_countries(scale = 50, country = "Germany", returnclass = "sf")
de <- st_transform(de, crs = "EPSG:4839")

st_crs(de) == st_crs(df_buli)

# club icons
icons_folder <- here("input", "team_icons")
icons_files <- list.files(icons_folder, pattern = ".png")
icons_tags <- glue::glue("<img src=\"{here(icons_folder, icons_files)}\" width=25>")
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
    aes(label = icon, 
        x = st_coordinates(st_centroid(geometry))[, "X"],
        y = st_coordinates(st_centroid(geometry))[, "Y"]),
    fill = NA, label.size = 0
  ) +
  scale_fill_identity() +
  labs(
    title = "What if everybody supported the football club<br>
    which is closest to their location?",
    subtitle = "<br>German Bundesliga and 2<sup>nd</sup> Bundesliga",
    caption = "Shapefile: Natural Earth, stadium locations: Wikidata. 
    Visualisation: Ansgar Wolsing (adapted from @VictimOfMaths)."
  ) +
  theme_void(base_family = "Inter") +
  theme(
    plot.background = element_rect(color = NA, fill = "grey87"),
    plot.margin = margin(6, 6, 6, 6),
    plot.title = element_markdown(
      family = "Oswald", size = 28, angle = 1.5, hjust = 0.5, lineheight = 1.15,
      margin = margin(t = 8, b = -16)),
    plot.subtitle = element_markdown(
      family = "Oswald", size = 16, angle = 1.5, hjust = 0.5, lineheight = 1,
      margin = margin(t = 0)),
    plot.caption = element_markdown(hjust = 0.5)
  )
ggsave(here("plots", "03-polygons-bundesliga-voronoi-map.png"), dpi = 500, width = 7.5, height = 9)

