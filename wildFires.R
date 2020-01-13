
# Australian Wildfires TidyTuesday

# Packages to install: 
# shiny
# tidyverse

# Get the Data

rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')
temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

# IF YOU USE THIS DATA PLEASE BE CAUTIOUS WITH INTERPRETATION
nasa_fire <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/MODIS_C6_Australia_and_New_Zealand_7d.csv')

# For JSON File of fires
url <- "http://www.rfs.nsw.gov.au/feeds/majorIncidents.json"
aus_fires <- sf::st_read(url)

## Or read in with tidytuesdayR package (https://github.com/thebioengineer/tidytuesdayR)

# Either ISO-8601 date or year/week works!
# Install via devtools::install_github("thebioengineer/tidytuesdayR")
#tuesdata <- tidytuesdayR::tt_load('2020-01-07') 
#tuesdata <- tidytuesdayR::tt_load(2020, week = 2)
#rainfall <- tuesdata$rainfall


# Plot Fires courtesy of Dean Marchiori
# Mapping NSW Current Incidents in R -------------------------------------------

library(sf)
library(mapview)
library(tidyverse)

#' Current Incidents Feed (GeoJSON)
#' This feed contains a list of current incidents from the NSW RFS, 
#' and includes location data and Major Fire Update summary information where available. 
#' Click through from the feed to the NSW RFS website for full details of the update. 
#' GeoJSON is a lightweight data standard that has emerged to support the sharing of 
#' information with location or geospatial data. 
#' It is widely supported by modern applications and mobile devices.

url <- "http://www.rfs.nsw.gov.au/feeds/majorIncidents.json"

fires <- st_read(url)
fires

mapview(fires)

#' Hacky way to get rid of points within geometry collections
fire_poly <- fires %>% 
  st_buffer(dist = 0) %>% 
  st_union(by_feature = TRUE)

mapview(fire_poly)

fires %>% 
  mutate(pubdate = as.character(pubDate),
         pubdate = as.Date(pubdate))


# Viz

#TODO: Recreate https://twitter.com/thomas_mock/status/1214255918533292037/photo/3

library(lubridate)
temperature %>% 
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(avg_temp = mean(temperature, na.rm = TRUE), cout = n())


