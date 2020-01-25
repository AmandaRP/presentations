# Australian Wildfires 
# This R script loads required libraries and datasets

# Load packages: 
library(shiny)
library(tidyverse)
library('latex2exp')
library(sf)
library(mapview)
library(lubridate)
library(magrittr)

# Get the Australian temperature data
temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

#Update temperature dataset
temperature %<>%
  drop_na() %>%
  mutate(year = year(date)) %>%
  filter(year >= 1930, year < 2019) %>%  #to avoid years with missing data
  filter(city_name %in% c("CANBERRA", "MELBOURNE", "SYDNEY")) #focus on NSW cities



#' Current Incidents Feed (GeoJSON)
#' This feed contains a list of current incidents from the NSW RFS, 
#' and includes location data and Major Fire Update summary information where available. 
#' Click through from the feed to the NSW RFS website for full details of the update. 
#' GeoJSON is a lightweight data standard that has emerged to support the sharing of 
#' information with location or geospatial data. 
#' It is widely supported by modern applications and mobile devices.

url <- "http://www.rfs.nsw.gov.au/feeds/majorIncidents.json"
fires <- st_read(url)

#' Hacky way to get rid of points within geometry collections
fire_poly <- fires %>% 
  st_buffer(dist = 0) %>% 
  st_union(by_feature = TRUE)





