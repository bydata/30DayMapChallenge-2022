library(tidyverse)
library(ggtext)
library(giscoR)
library(sf)
library(patchwork)
library(here)

# Member states of the EU
eu_countries <- 
  c("Netherlands", "Belgium", "Luxembourg", "Sweden", "France", "Germany", "Italy",
    "Spain", "Portugal", "Greece", "Ireland", "Austria", "Finland", "Denmark",
    "Estonia", "Latvia", "Lithuania", "Poland", "Czechia", "Slovakia", "Slovenia",
    "Croatia", "Romania", "Bulgaria", "Malta", "Cyprus", "Hungary")
length(eu_countries)

world <- giscoR::gisco_countries
st_crs(world)
europe <- world %>% 
  mutate(continent = countrycode::countrycode(ISO3_CODE, "iso3c", "continent"),
         continent = ifelse(ISO3_CODE == "CYP", "Europe", continent)) %>% 
  st_crop(c("xmin" = -13.5, "ymin" = 31.5, "xmax" = 42, "ymax" = 69)) %>% 
  filter(continent == "Europe" |
          # NAME_ENGL %in% c("Turkey", "Syria", "Lebanon", "Jordan", "Israel", "Iraq")
           NAME_ENGL == "Turkey"
         )

# Map
p <- europe %>% 
  ggplot() +
  ggfx::with_shadow(
    geom_sf(
      aes(fill = case_when(
        NAME_ENGL == "Ukraine" ~ NAME_ENGL,
        NAME_ENGL %in% eu_countries ~ "EU",
        TRUE ~ "Other"),
        color = NAME_ENGL == "Ukraine"),
      size = 0.2),
    x_offset = 2, y_offset = 2
    ) +
  ggfx::with_shadow(
    data = ~subset(., NAME_ENGL == "Ukraine"),
    geom_sf(
      aes(fill = case_when(
        NAME_ENGL == "Ukraine" ~ NAME_ENGL,
        NAME_ENGL %in% eu_countries ~ "EU",
        TRUE ~ "Other"),
        color = NAME_ENGL == "Ukraine"),
      size = 0.2),
    x_offset = 2, y_offset = 2
  ) +
  scale_color_manual(values = c("grey90", "#0057B8")) +
  scale_fill_manual(values = c("Other" = "grey78", "Ukraine" = "#FFD700", "EU" = "#0057B8")) +
  guides(fill = "none", color = "none") +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(color = "grey97", fill = "grey97")
  )
p

# Country indicators ===========================================================
#' Source: Eurostat

# Reads the data frame and remove flags (letters)
read_and_prep_data <- function(x) {
  read_tsv(x, na = ":") %>% 
    mutate(across(-c(1), function(x) as.numeric(str_remove_all(x, "[a-z\\s]")))) %>% 
    separate(1, sep = ",", into = c("freq", "indicator", "iso2c")) %>% 
    mutate(country = countrycode::countrycode(iso2c, "iso2c", "country.name"),
           country = ifelse(iso2c == "EU27_2020", "EU27", country)) %>% 
    pivot_longer(cols = -c(freq, indicator, iso2c, country), names_to = "year", values_to = "value")
}

# Plost the indicator chart
plot_country_info <- function(df, label, aes = c("col", "point")) {
  p <- df %>% 
    filter(country %in% eu_countries | iso2c == "UA") %>% 
    filter(year == max(year)) %>% 
    mutate(iso2c = fct_reorder(iso2c, -value)) %>% 
    ggplot(aes(iso2c, value, fill = iso2c == "UA")) +
    scale_fill_manual(values = c("#0057B8", "#FFD700")) +
    guides(fill = "none") +
    labs(title = label) +
    theme_void(base_family = "Roboto Condensed") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 12, color = "grey20", face = "bold"),
      panel.background = element_rect(color = "grey60", fill = NA)
    )
  aes2 <- match.arg(aes)
  if (aes2 == "col") {
    p  + geom_col(width = 0.75) 
  } else if (aes2 == "point") {
    p + 
      geom_vline(aes(xintercept = iso2c), size = 0.2, color = "grey80") +
      geom_point(aes(col = iso2c == "UA"),
                 shape = 21,  size = 2) +
      scale_color_manual(values = c("white", "#0057B8")) +
      guides(color = "none")
  }
}

# Get the data for the indicators ----------------------------------------------

## Population
#' Source: https://ec.europa.eu/eurostat/databrowser/bookmark/40fd4e5f-3ace-4618-86a5-e9ecfea67ac2?lang=en
pop <- read_and_prep_data("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/TPS00001/?format=TSV&compressed=false")

## Median age
url_median_age <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/DEMO_PJANIND/A.MEDAGEPOP.EU27_2020+EU28+EU27_2007+EA19+EA18+BE+BG+CZ+DK+DE+DE_TOT+EE+IE+EL+ES+FR+FX+HR+IT+CY+LV+LT+LU+HU+MT+NL+AT+PL+PT+RO+SI+SK+FI+SE+EEA31+EEA30_2007+EEA28+EFTA+IS+LI+NO+CH+UK+ME+MK+AL+RS+TR+AD+BY+BA+XK+MD+MC+RU+SM+UA+AM+AZ+GE/?format=TSV&startPeriod=2012&endPeriod=2021"
age <- read_and_prep_data(url_median_age)

## GDP per capita
# Source: Worldbank (https://data.worldbank.org/indicator/NY.GDP.PCAP.CD)
url_gdp <- "https://api.worldbank.org/v2/en/indicator/NY.GDP.PCAP.CD?downloadformat=csv"
file_gdp_zip <- here("data", "worldbank_gdp.zip")
file_gdp <- here("data", "worldbank_gdp", "API_NY.GDP.PCAP.CD_DS2_en_csv_v2_4697301.csv")
download.file(url_gdp, destfile = file_gdp_zip)
unzip(file_gdp_zip, exdir = here("data",  "worldbank_gdp"))
gdp <- 
  read_csv(file_gdp, col_names = TRUE, skip = 3) %>% 
  mutate(freq = NA,
         iso2c = countrycode::countrycode(`Country Name`, "country.name", "iso2c")) %>%  
    rename(country = `Country Name`, indicator = `Indicator Name`) %>%
    select(freq, indicator, iso2c, country, matches("\\d{4}")) %>% 
  pivot_longer(cols = -c(freq, indicator, iso2c, country), names_to = "year", 
               values_to = "value")

## Country area
country_area <- europe %>% 
  filter(NAME_ENGL %in% eu_countries | NAME_ENGL == "Ukraine") %>% 
  mutate(area = st_area(geometry),
         area_m2 = as.numeric(area),
         area_km2 = area_m2 / 1000^2,
         iso2c = countrycode::countrycode(NAME_ENGL, "country.name", "iso2c"),
         year = 2022) %>% 
  st_drop_geometry() %>% 
  select(country = NAME_ENGL, iso2c, year, value = area_km2)


# Indicator charts
p1 <- plot_country_info(pop, label = "Population")
p2 <- plot_country_info(age, "Median Age", aes = "point") +
  coord_cartesian(ylim = c(30, NA))
p3 <- plot_country_info(gdp, "GDP per capita", aes = "col")
p4 <- plot_country_info(country_area, "Area")

# Combined chart
plot_design = "
1112
1112
"
p + (p4 / p1 / p2 / p3) +
  plot_annotation(
    title = "If and when <span style='color:#FFD700'>Ukraine</span> joined the 
    <span style='color:#0057B8'>European Union</span>",
    subtitle = "Ukraine would be the country with the largest area, 
    the 5th largest and one of the youngest population, but also the country
    with the lowest GDP per capita.",
    caption = "Sources: Eurostat, Worldbank (2021). Visualisation: Ansgar Wolsing",
    theme = theme(
      plot.title = element_markdown(
        family = "Roboto Condensed", face = "bold", size = 24,
        color = "grey9", hjust = 0.5),
      plot.subtitle = element_textbox(
        family = "Roboto Condensed", width = 1, lineheight = 1.25, size = 14,
        hjust = 0.5, halign = 0.5)
    )
  ) +
  plot_layout(design = plot_design) &
  theme(
    plot.background = element_rect(color = "grey62", fill = "grey62"),
    plot.margin = margin(rep(6, 4))
  )
ggsave(here("plots", "05-ukraine-in-eu-combined.png"), width = 7.5, height = 8)
