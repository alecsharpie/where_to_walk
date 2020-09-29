# load packages
library(tidyverse)
library(lubridate)
library(magrittr)

# data source
#https://data.melbourne.vic.gov.au/browse?q=pedestrian&sortBy=relevance

# read in pedestrian count data
ped_count <-
  read.csv(
    "/Users/admin/Desktop/Alec/Web data/Where to walk - V2/data/Pedestrian_Counting_System_2009_to_Present_counts_per_hour.csv",
    header = TRUE,
    stringsAsFactors = FALSE
  )


##### Initial Explore #####

# check how many days data each sensor has
sensors <- ped_count %>%
  group_by(Sensor_ID, Sensor_Name) %>%
  summarise(days = (n() / 24)) %>%
  arrange(desc(days))

# check how many sensors there were in each year
date_avg %>%
  group_by(year(date)) %>%
  summarise(avg = mean(n))

#take a sample of the dataset to visualise
ped_explore <- ped_count %>%
  sample_frac(0.01)

# order factors

# order Day of the week factor
ped_explore$Day <- factor(ped_explore$Day,
                          levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                          ordered = TRUE)

# order Time factor
ped_explore$Time <- factor(ped_explore$Time,
                           levels = as.character(c(0:23)),
                           ordered = TRUE)

# order Month factor
ped_explore$Month <- factor(ped_explore$Month,
                            levels = as.character(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")),
                            ordered = TRUE)


#by year = not visually significant
boxplot(Hourly_Counts ~ Year, data = ped_count_explore)

#by month = not visually significant
boxplot(Hourly_Counts ~ Month, data = ped_count_explore)

# by date of the month = not visually significant
boxplot(Hourly_Counts ~ Mdate, data = ped_count_explore)

#by month = visually significant
boxplot(Hourly_Counts ~ Day, data = ped_count_explore)

# by date of the month = visually significant
boxplot(Hourly_Counts ~ Time, data = ped_count_explore)


# Day of the week
ggplot()+
  geom_boxplot(data = ped_explore, aes(x = Day, y = Hourly_Counts), outlier.shape = NA)+
  scale_y_continuous(limits = c(0,5000))

# Time of day
ggplot()+
  geom_boxplot(data = ped_explore, aes(x = Time, y = Hourly_Counts), outlier.shape = NA)+
  geom_smooth()+
  scale_y_continuous(limits = c(0,5000))+
  coord_polar()

# Month
ggplot()+
  geom_point(data = ped_explore, aes(x = Month, y = Hourly_Counts))+
  geom_smooth(data = ped_explore, aes(x = Month, y = Hourly_Counts))+
  scale_y_continuous(limits = c(0,5000))


# explore hourly counts
par(mfrow = c(1, 1))
hist(ped_explore$Hourly_Counts, breaks = 1000)

