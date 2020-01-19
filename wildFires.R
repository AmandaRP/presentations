
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
temp_avgs <- temperature %>% 
  mutate(year = year(date)) %>%
  drop_na() %>%
  group_by(year) %>%
  summarize(avg_temp = mean(temperature, na.rm = TRUE), count = n()) 

# 2019 seems to be incomplete, so we should not use. Visualize:
temp_avgs %>%
  ggplot(aes(year, count)) + 
  geom_point() 

# Filter out years with fewer data points
library(magrittr)
temp_avgs %<>% filter(year >= 1950, year < 2019)

overall_avg_temp <- temp_avgs %>% 
  summarize(overall_avg_temp = mean(avg_temp)) %>% 
  unlist()

library('latex2exp')

temp_avgs %>% 
  mutate(temp_minus_mean = avg_temp - overall_avg_temp) %>%
  ggplot(aes(year, temp_minus_mean, fill = temp_minus_mean<0)) + 
  geom_col() +
  labs(x = element_blank(), 
       y = element_blank(),
       title = "Australia is getting warmer",
       caption = "Source: Australian Government Bureau of Meteorology") +
  theme_minimal() +
  scale_y_continuous(breaks = c(-1, -0.5, 0.5, 1),
                   labels = c(parse(text = TeX('$-1.0^o$')), 
                              parse(text = TeX('$-0.5^o$')), 
                              parse(text = TeX('$+0.5^o$')), 
                              parse(text = TeX('$+1.0^o$')))) +
  theme(#axis.text.y = element_blank(),
        plot.background = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype = "dashed", color = "grey"),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "none",
        plot.caption = element_text(color = "darkgrey")) +
  annotate("text", 
           x = 1962, 
           y = 0.7, 
           color = "darkgrey",
           label = "Annual temperature (degrees Celcius)\nabove or below the 1950-2018 average") 


## rainfall

rainfall %>% 
  summarize(m = min(year), max = max(year), cnt = n())


#look at timeline by city. TODO: Need to impute missing data.
rainfall %>%
  filter(year >= 1950, year < 2020) %>%
  replace_na(list(rainfall = 0)) %>%
  #select(city_name) %>%
  group_by(city_name, year) %>%
  summarize(avg = mean(rainfall), count = n()) %>%
  #distinct()
  ggplot(aes(year,avg, group = city_name, color = city_name)) +
  geom_line()
  


rain_avgs <- rainfall %>% 
  drop_na() %>%
  group_by(year) %>%
  summarize(avg_rain = mean(rainfall, na.rm = TRUE), count = n()) 

rain_avgs %>%
  filter(year >= 1950, year < 2020) %>%
  ggplot(aes(year, count)) +
  geom_point()

rain_avgs %<>% filter(year >= 1950, year < 2020)

overall_avg_rain <- rain_avgs %>% 
  summarize(overall_avg_rain = mean(avg_rain)) %>% 
  unlist()

rain_avgs %>% 
  mutate(rain_minus_mean = avg_rain - overall_avg_rain) %>%
  ggplot(aes(year, rain_minus_mean, fill = rain_minus_mean>0)) + 
  geom_col() +
  labs(x = element_blank(), 
       y = element_blank(),
       title = "Australia is getting dryer?",
       caption = "Source: Australian Government Bureau of Meteorology") +
  theme_minimal() +
  #scale_y_continuous(breaks = c(-1, -0.5, 0.5, 1),
  #                   labels = c(parse(text = TeX('$-1.0^o$')), 
  #                              parse(text = TeX('$-0.5^o$')), 
  #                              parse(text = TeX('$+0.5^o$')), 
  #                              parse(text = TeX('$+1.0^o$')))) +
  theme(#axis.text.y = element_blank(),
    plot.background = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "grey"),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    plot.caption = element_text(color = "darkgrey")) #+
  #annotate("text", 
  #         x = 1962, 
  #         y = 0.7, 
  #         color = "darkgrey",
  #         label = "Annual temperature (degrees Celcius)\nabove or below the 1950-2018 average") 

