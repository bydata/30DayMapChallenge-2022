library(tidyverse)
library(geofacet)
library(ggtext)
library(here)

#' Source for chart peak positions:
#' https://en.wikipedia.org/wiki/Blue_(Da_Ba_Dee)#Charts 
charts_peak_position <- tribble(
  ~name, ~peak_position,
  "Australia", "1",
  "Austria", "1",
  "Belgium", "1",
  "Canada", "1",
  "Denmark", "1",
  "Finland", "1",
  "France", "1",
  "Germany", "1",
  "Greece", "1",
  "Hungary", "1",
  "Ireland", "1",
  "Lithuania", "1",
  "Netherlands", "1",
  "Netherlands", "1",
  "New Zealand", "1",
  "Norway", "1",
  # "Scotland", "1",
  "Sweden", "1",
  "Switzerland", "1",
  "Great Britain and Northern Ireland", "1",
  "Spain", "2",
  "Croatia", "3",
  "Italy", "3",
  "Iceland", "4",
  "United States of America", "6"
)

# Check if all countries match
nrow(charts_peak_position)
charts_peak_position %>% 
  anti_join(world_countries_grid1, by = "name")

# Countries in grid
grid_preview(world_countries_grid1)
world_countries_grid1$name

## the grid is lacking Taiwan, let's add it below South Korea
head(world_countries_grid1)
world_countries_grid2 <- world_countries_grid1 %>% 
  add_row(name = "Taiwan", code_alpha3 = "TWN", code_country = "158", 
          code_iso_3166_2 = "ISO 3166-2:TW", col = 25, row = 8)

grid_preview(world_countries_grid2) +
  theme_void()


charts_peak_position %>% 
  right_join(world_countries_grid2, by = "name") %>% 
  mutate(peak_position_number1 = ifelse(peak_position == 1, "I'm Blue", "Not Blue"),
         peak_position_number1 = replace_na(peak_position_number1, "Not Blue"),
         country_code = str_remove(code_iso_3166_2, "ISO 3166-2:")) %>% 
  ggplot(aes(col, -row)) +
  geom_tile(aes(fill = peak_position_number1),
            color = "white") +
  geom_text(
    # data = ~subset(., !is.na(peak_position)),
    data = ~subset(., peak_position_number1 == "I'm Blue"),
    aes(label = country_code),
    color = "white", size = 2, fontface = "bold", family = "Roboto Condensed") +
  scale_fill_manual(values = c("I'm Blue" = "#273F9D", "Not Blue" = "grey72")) +
  # colorspace::scale_fill_discrete_sequential(
  #   palette = "Blues", na.value = "grey72", rev = FALSE, 
  #   labels = function(x) ifelse(is.na(x), "Not Blue", x)) +
  coord_fixed() +
  labs(
    title = "Countries in which Eiffel65's<br>
    <span style='font-size: 28pt'>Blue (Da Ba Dee)</span><br>
    peaked at No. 1",
    caption = "Source: Wikipedia, grid: adapted from geofacets R package.
    Visualisation: Ansgar Wolsing",
    fill = NULL
  ) + 
  theme_void(base_family = "Roboto Condensed") +
  theme(
    plot.background = element_rect(color = "#E0F0F0", fill = "#D4E9F8"),
    legend.position = "top",
    legend.key.size = unit(3, "mm"),
    plot.margin = margin(4, 4, 4, 4),
    strip.text = element_text(size = 12, margin = margin(t = 4, b = 6), face = "bold"),
    plot.title = element_markdown(
      family = "Coda", hjust = 0.5, color = "#273F9D", lineheight = 1.25,
      margin = margin(b = 8)),
    plot.caption = element_markdown()
    # strip.background = element_rect(color = "grey40", fill = "grey40")
  )
ggsave(here("plots", "18-blue-eiffel65.png"), width = 6, height = 6)
