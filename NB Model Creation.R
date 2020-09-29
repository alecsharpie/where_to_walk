# load packages
library(tidyverse)
library(lubridate)
library(MASS)
library(magrittr)
library(tibble)
library(car)
library(broom)
library(MXM)

# read in pedestrian count data
ped_count <- read.csv("data/Pedestrian_Counting_System_2009_to_Present_counts_per_hour.csv", header = TRUE, stringsAsFactors = FALSE)

#read in sensor location data
senloc <- read.csv("https://www.learnatschoolschool.com/wp-content/uploads/2020/01/Pedestrian_Counting_System_-_Sensor_Locations.csv",header=TRUE)

#find sensors that havent been removed
active_sensors <- senloc %>%
  filter(status == "A") %>%
  pull(sensor_id)
  
# number of hours in two years
two_years <- 24*365*2

# find sensors with adequate data (more than 2 years) that havent been removed / inactive
adequate_sensors <- ped_count %>%
  group_by(Sensor_ID) %>%
  summarise(n = n()) %>%
  filter(n > two_years) %>%
  filter(Sensor_ID %in% active_sensors) %>%
  arrange(desc(n)) %>%
  pull(Sensor_ID) %>%
  as.character()

#number of adequate sensors
length(adequate_sensors)

#process data for model, convert columns to correct type, filter for adaquate data, add a column for the effect of covid
melb_data <- ped_count %>%
  mutate(Date_Time = mdy_hms(Date_Time)) %>%
  mutate(date = date(Date_Time)) %>%
  mutate(Time = as.character(Time)) %>%
  mutate(Sensor_ID = as.character(Sensor_ID)) %>%
  filter(Sensor_ID %in% adequate_sensors) %>%
  mutate(Covid = if_else(Date_Time > date("2020-03-15"), 1, 0)) %>%
  drop_na() %>%
  sample_frac(0.01) #%>% # while testing i only used a sample to save time, but i ran the final model on all of the data
# select(date, Day, Time, Sensor_ID, Hourly_Counts)

#convert time to numeric
melb_data$Time <- as.numeric(melb_data$Time)

#nest data so it is grouped by sensor_ID
by_sensor_id <- melb_data %>% 
  group_by(Sensor_ID) %>% 
  nest()

##### create model #####

# best model!
# treat Time as a circular variable
sensor_model_nb_circ_time <- function(df) {
  glm.nb(Hourly_Counts ~ Covid + Day + cos(pi*Time/12) + sin(pi*Time/12), data = df)
}

# create our model and add it to each row of the dataframe
by_sensor_id <- by_sensor_id %>% 
  mutate(model = map(data, sensor_model_nb_circ_time)) %>%
  mutate(glance = map(model, broom::glance))

#check one of our models
by_sensor_id$model[[1]]

##### use sensor_model_nb_circ_time for all sensors
# For each sensor: predict a hourly count for each time for each day
# 168 predictions per sensor

#predict resulting values for each time and day
add_all_predictions <- function(mod){
  
  # create vectors to create a table for all possible time/days combos
  Day <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  Time <- 0:23
  Covid <- c(0, 1)
  
  # create the tibble for all possible time/days combos
  to_pred <- tidyr::crossing(Day, Time, Covid)
  
  # create predictions
  predictions <- exp(Predict(mod, newdata = to_pred, interval = "confidence"))

  # add predictions to factor combos
  all_preds <- cbind(to_pred, predictions)
  
  return(all_preds)
}

# create predictions for our model for each row of the dataframe to predicted
by_sensor_id <- by_sensor_id %>% 
  mutate(predictions = map(model, add_all_predictions))

# unnest data to have one dataset for all sensors' predictions
all_predictions <- by_sensor_id %>%
  dplyr::select("sensor_id" = Sensor_ID, predictions) %>%
  unnest(predictions)

write.csv(all_predictions, "processed_data/predictions_for_all_sensors.csv")



