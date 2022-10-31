library(tidyverse)
library(sf)
library(ggtext)
library(cartogram)
library(here)

#' Beer consumption per capita
#' Source: Our World in Data, WHO, Global Health Observatory
#' https://ourworldindata.org/grapher/beer-consumption-per-person?time=latest&region=Europe
#' Average annual per capita beer consumption, measured in liters of pure alcohol.
#' Beer contains around 5% of pure alcohol per volume so that one liter of beer 
#' contains 0.05 liters of pure alcohol. This means that 5 liters of pure alcohol 
#' equals 100 liters of beer.

beer <- read_csv(here("data", "beer-consumption-per-person.csv"), 
                 name_repair = janitor::make_clean_names)
beer_prep <- beer %>% 
  rename(liters_pure_alc_pp = 4) %>% 
  mutate(liters_beer_pp = liters_pure_alc_pp * 20) %>% 
  filter(year == max(year)) %>% 
  arrange(-liters_beer_pp) 
head(beer_prep)


# World shapes
library(maptools)
data(wrld_simpl)

world <- st_as_sf(wrld_simpl) %>% 
  # st_transform("EPSG:3035")
  st_transform("+proj=moll")
st_crs(world)

# Europe
europe <- wrld_simpl %>% 
  st_as_sf() %>% 
  filter(REGION == 150) %>% 
  st_transform("+proj=moll")
st_crs(europe)

# Join world sf with beer consumption
head(world)
head(europe)
head(beer_prep)
europe %>% 
  st_drop_geometry() %>% 
  anti_join(beer_prep, by = c("ISO3" = "code")) %>% 
  select(ISO3, NAME)

europe_beer <- europe %>% 
  inner_join(beer_prep, by = c("ISO3" = "code")) %>% 
  select(ISO2, ISO3, entity, year, POP2005, liters_pure_alc_pp, liters_beer_pp, LON, LAT, geometry) %>% 
  filter(entity != "Russia")

europe_beer_carto <- cartogram_dorling(europe_beer, weight = "liters_beer_pp")
europe_beer_carto <- cartogram_cont(europe_beer, weight = "liters_beer_pp") 
ggplot(europe_beer_carto) +
  geom_sf(fill = "#ECC421", color = "grey99", size = 1) +
  geom_sf_text(aes(label = paste(str_wrap(entity, 10), "\n", round(liters_beer_pp), "l")), 
                   size = 3, family = "Roboto Condensed") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#c9c8c3")
  )
ggsave(here("plots", "15-food-drinks-beer-cartogramm-cont.png"), width = 7, height = 7)


# Image source: https://pixabay.com/photos/beer-alcohol-drink-beverage-bar-2536111/
# Image source: Photo by <a href="https://unsplash.com/@stefanbc?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText">Stefan Cosma</a> on <a href="https://unsplash.com/s/photos/beer-bottle-cap?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText">Unsplash</a>
# Font: https://www.fontspace.com/ss-nickson-one-font-f28811

p <- ggplot(europe_beer_carto) +
  geom_sf(fill = "#ECC421", color = "grey99", size = 1)
ggplot_build(p) %>% View()

plot_layer_data <- layer_data(p) %>% 
  bind_cols(select(st_drop_geometry(europe_beer), entity, ISO2, liters_beer_pp)) %>% 
  st_as_sf() %>% 
  st_centroid() %>% 
  mutate(coord = st_coordinates(geometry),
         x = coord[,"X"], y = coord[,"Y"]) %>% 
  # arrange to control which country gets plotted on top of the others
  arrange(liters_beer_pp)


xmin <- first(plot_layer_data$xmin) - 10000
xmax <- first(plot_layer_data$xmax) + 10000
ymin <- first(plot_layer_data$ymin) - 10000
ymax <- first(plot_layer_data$ymax) + 10000

p_edited <- plot_layer_data %>% 
  mutate(
    image = sprintf(
      "<img src='input/beer-bottle-cap.png' width=%d>", round(liters_beer_pp / 1.25)),
    label_iso2 = sprintf(
      "<b style='font-size: 14pt'>%s</b><br>%s L", ISO2, round(liters_beer_pp))) %>% 
  ggplot(aes(x, y)) +
  geom_richtext(
    aes(label = image),
    fill = NA, label.size = 0
  ) +
  geom_richtext(
    aes(label = label_iso2),
    fill = alpha("#DC392F", 0.5), color = "white", 
    # fill = alpha("white", 0.5), color = "grey4", 
    label.size = 0.2, family = "Roboto Condensed",
    vjust = 1.1, size = 3.5
  ) +
  scale_size_continuous(range = c(1, 30)) +
  coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax), clip = "off") +
  labs(
    title = "Who Drinks How Much Beer?",
    subtitle = "Beer Consumption per person in European countries (liters)",
    caption = "Source: Our World in Data, WHO, Global Health Observatory (2019).<br>
    Visualisation: Ansgar Wolsing (photo: Stefan Cosma, unsplash.com)"
  ) +
  theme_void(base_family = "Lato") +
  theme(
    plot.background = element_rect(color = "#c9c8c3", fill = "#c9c8c3"),
    plot.title = element_text(family = "SS Nickson One", hjust = 0.5, size = 36),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_markdown(hjust = 0.5, lineheight = 1.1),
    plot.margin = margin(rep(2, 4))
  )
ggsave(here("plots", "15-food-drinks-beer-cartogramm-cont.png"), width = 7, height = 8)
