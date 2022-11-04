library(tidyverse)

# Read Datawrapper countrycode for map type "World population squares"
ccdw <- read_csv(here("data", "countrycodes-datawrapper.csv"))

ccdw_names <- ccdw %>% 
  mutate(name = countrycode::countrycode(iso, origin = "iso3c", destination = "country.name")) 
ccdw_names

flags <- read_csv2(here("data", "flags.csv"))
flags_prep <- flags %>% 
  transmute(
    iso3 = countrycode::countrycode(name, origin = "country.name", destination = "iso3c"),
    green,
    name)

flags_prep %>% 
  filter(is.na(iso3))


# Do the country codes match?
# Which countries are missing from the flags dataset?
ccdw_names %>% 
  anti_join(flags_prep, by = c("iso" = "iso3")) %>% View()

# manually code the flags for missing countries
additional_flags <- tribble(
  ~iso3, ~green, ~name,
  "ARM",      0, "Armenia",
  "AZE",      1, "Azerbaijan",
  "BIH",      0, "Bosnia & Herzegovina",
  "BLR",      1, "Belarus",
  "COM",      1, "Comores",
  "CZE",      0, "Czechia",
  "ERI",      1, "Eritrea",
  "EST",      0, "Estonia",
  "GEO",      0, "Georgia",
  "HRV",      0, "Croatia",
  "KAZ",      0, "Kazakhstan",
  "KGZ",      0, "Kyrgyzstan",
  "LTU",      1, "Lithuania",
  "LVA",      0, "Latvia",
  "MAC",      0, "Macao SAR China",
  "MDA",      0,  "Moldova",
  "MKD",      0,  "North Macedonia",
  "MNE",      1,  "Montenegro",
  "NAM",      1,  "Namibia",
  "NCL",      1,  "New Caledonia",
  "PRY",      0,  "Paraguay",
  "PSE",      1,  "Palestinian Territories",
  "SLB",      0,  "Solomon Islands",
  "SRB",      0,  "Serbia",
  "SSD",      1,  "South Sudan",
  "SVK",      0,  "Slovakia",
  "SVN",      0,  "Slovenia",
  "TJK",      1,  "Tajikistan",
  "TKM",      1,  "Turkmenistan",
  "TLS",      0,  "Timor-Leste",
  "UKR",      0,  "Ukraine",
  "UZB",      1,  "Uzbekistan",
  "YEM",      0,  "Yemen"
  
)

flags_prep <- flags_prep %>% 
  bind_rows(additional_flags) %>% 
  mutate(green = ifelse(green, "green", "not green"))

write_csv(flags_prep, here("output", "flags-prep.csv"))
