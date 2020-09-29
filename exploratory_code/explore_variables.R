library(tidyverse)
library(lubridate)
library(magrittr)

# data source
#https://data.melbourne.vic.gov.au/browse?q=pedestrian&sortBy=relevance

# read in pedestrian count data
ped_count <- read.csv("/Users/admin/Desktop/Alec/Web data/Where to walk - V2/data/Pedestrian_Counting_System_2009_to_Present_counts_per_hour.csv", 
                      header = TRUE, 
                      stringsAsFactors = FALSE)

# check how many days data each sensor has
sensors <- ped_count%>%
  group_by(Sensor_ID, Sensor_Name)%>%
  summarise(days = (n()/24))%>%
  arrange(desc(days))

# check how many sensors there were in each year
date_avg %>%
  group_by(year(date)) %>%
  summarise(avg = mean(n))

#take a sample of the dataset to visualise
ped_count_explore <- ped_count %>%
  sample_frac(0.01)

#by year = not visually significant
boxplot(Hourly_Counts~Year, data = ped_count_explore) 

#by month = not visually significant
boxplot(Hourly_Counts~Month, data = ped_count_explore)

# by date of the month = not visually significant
boxplot(Hourly_Counts~Mdate, data = ped_count_explore)

#by month = visually significant
boxplot(Hourly_Counts~Day, data = ped_count_explore)

# by date of the month = visually significant
boxplot(Hourly_Counts~Time, data = ped_count_explore)





