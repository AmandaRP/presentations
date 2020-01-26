# Australian Wildfires 
# This R script loads required libraries and datasets

# Load packages: 
library(shinythemes)
library(tidyverse)
library(latex2exp)
library(sf)
library(mapview)
library(lubridate)
library(magrittr)
library(gghighlight)

# Get the Australian temperature data
temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

#Update temperature dataset
temperature %<>%
  drop_na() %>%
  mutate(year = year(date)) %>%
  filter(year >= 1930, year < 2019) %>%  #to avoid years with missing data
  filter(city_name %in% c("CANBERRA", "MELBOURNE", "SYDNEY")) #focus on NSW cities


rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')

#Verify that rainfall was measured for all 12 months in the year
valid_years <- rainfall %>% 
  drop_na() %>%
  group_by(city_name, year, month) %>%
  count() %>%
  group_by(city_name, year) %>%
  count() %>% #Count number of months were represented
  filter(n == 12) %>%
  select(city_name, year)

#look at timeline by city. 
rain_by_year <- rainfall %>%
  inner_join(valid_years, by = c("city_name", "year")) %>%
  filter(year >= 1930, year <= 2018) %>%
  filter(city_name %in% c("Canberra", "Melbourne", "Sydney")) %>%
  drop_na() %>%
  group_by(city_name, year, month) %>%
  summarize(monthly_avg = mean(rainfall)) %>% # monthly average
  group_by(city_name, year) %>%
  summarize(avg = mean(monthly_avg))  # yearly average

p <- rain_by_year %>%
  ggplot(aes(year,avg, group = city_name, color = city_name)) +
  geom_line() + 
  labs(title = "Annual Rainfall",
       x = element_blank(), 
       y = "Millimeters",
       caption = "Source: Australian Government Bureau of Meteorology") +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed", color = "grey"),
        panel.grid.minor.x = element_blank(),
        axis.title.y = element_text(color = "darkgrey"),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.caption = element_text(color = "darkgrey"))

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





