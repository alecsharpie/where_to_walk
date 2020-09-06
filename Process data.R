library(tidyverse)
library(lubridate)

#https://data.melbourne.vic.gov.au/browse?q=pedestrian&sortBy=relevance

# read in pedestrian count data
ped_count <- read.csv("data/Pedestrian_Counting_System_2009_to_Present_counts_per_hour.csv", header = TRUE, stringsAsFactors = FALSE)

# read in locations of sensors
sensor_locations <- read.csv("data/Pedestrian_Counting_System_-_Sensor_Locations.csv", header = TRUE, stringsAsFactors = FALSE)

#explore variables

#by year = not important
by_year <- ped_count%>%
  group_by(Year)%>%
  summarise(avg = median(Hourly_Counts, na.rm = TRUE))
by_year

#by month = not important
by_month <- ped_count%>%
  group_by(Month)%>%
  summarise(avg = median(Hourly_Counts, na.rm = TRUE))
by_month

#by day of month = not important
by_mdate <- ped_count%>%
  group_by(Mdate)%>%
  summarise(avg = median(Hourly_Counts, na.rm = TRUE))
by_mdate

#by day is important
by_day <- ped_count%>%
  group_by(Day)%>%
  summarise(avg = median(Hourly_Counts, na.rm = TRUE))
by_day

#by day is important
by_time <- ped_count%>%
  group_by(Time)%>%
  summarise(avg = median(Hourly_Counts, na.rm = TRUE))
by_time

# check how many days data each sensor has
sensors <- ped_count%>%
  group_by(Sensor_ID, Sensor_Name)%>%
  summarise(days = (n()/24))%>%
  arrange(desc(days))

# check how many sensors there were in each year
date_avg %>%
  group_by(year(date)) %>%
  summarise(avg = mean(n))


#### read in weather data ####

weather_data <- read.csv("data/melbourne_weather_2009-2020.csv", header = TRUE, stringsAsFactors = FALSE)
