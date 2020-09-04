library(tidyverse)
library(lubridate)
library(magrittr)
library(scales)

# read in pedestrian count data
ped_count <- read.csv("data/Pedestrian_Counting_System_2009_to_Present_counts_per_hour.csv", header = TRUE, stringsAsFactors = FALSE)


# create number of hours in two years
two_years <- 24*365*2

# find sensors with adaquate data (more than 2 years of data)
by_sensor <- ped_count %>%
  group_by(Sensor_ID) %>%
  summarise(n = n()) %>%
  filter(n > two_years) %>%
  arrange(desc(n)) %>%
  pull(Sensor_ID) %>%
  as.character()

#process data for model
melb_data <- ped_count %>%
  mutate(Date_Time = mdy_hms(Date_Time)) %>%
  mutate(date = date(Date_Time)) %>%
  mutate(Time = as.character(Time)) %>%
  mutate(Sensor_ID = as.character(Sensor_ID)) %>%
  filter(Sensor_ID %in% by_sensor) %>%
  drop_na() %>%
  sample_frac(0.001) #%>%
# select(date, Day, Time, Sensor_ID, Hourly_Counts)


# creating working object
ped_explore <- melb_data

# order Day of the week factor
ped_explore$Day <- factor(ped_explore$Day,
                          levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                          ordered = TRUE)

# order Time factor
ped_explore$Time <- factor(ped_explore$Time,
                           levels = as.character(c(0:23)),
                           ordered = TRUE)

# order Month factor
#ped_explore$Month <- factor(ped_explore$Month,
#                            levels = as.character(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")),
#                            ordered = TRUE)



# Day of the week
ggplot()+
  geom_boxplot(data = ped_explore, aes(x = Day, y = Hourly_Counts), outlier.shape = NA)+
  scale_y_continuous(trans = "log10",
                     limits = c(NA, 100000),
                     labels = comma)
  #coord_trans(y="log10")

warnings()



#create custom data for radial area plot
by_day <- ped_explore %>%
  group_by(Day)%>%
  summarise('tf_percentile' = quantile(Hourly_Counts, 0.25),
            'median' = quantile(Hourly_Counts, 0.5),
            'sf_percentile' = quantile(Hourly_Counts, 0.75))#%>%
  #add_row('Day' = "Monday", 'tf_pertcentile' = )


ggplot(data = by_day)+
  geom_ribbon(aes(x = Day, ymin = tf_percentile, ymax = sf_percentile))


summary(ped_explore$Hourly_Counts)

# Time of day

# replace 20 with input$time for reactive shiny
ggplot()+
  geom_boxplot(data = ped_explore, aes(x = Time, y = Hourly_Counts), outlier.shape = NA)+
  geom_boxplot(data = ped_explore[which(ped_explore$Time == 20),], aes(x = Time, y = Hourly_Counts), outlier.shape = NA, color = "green")+
  scale_y_continuous(trans = "log2",
                     limits = c(NA, 100000),
                     labels = comma)+
  coord_polar()+
  theme_light()

# Time of Day
ggplot()+
  geom_jitter(data = ped_explore, aes(x = Time, y = Hourly_Counts), width = 0.1)+
  scale_y_continuous(limits = c(0,5000))+
  coord_polar()+
  theme_light()



# Month
#ggplot()+
#  geom_boxplot(data = ped_explore, aes(x = Month, y = Hourly_Counts), outlier.shape = NA)+
#  scale_y_continuous(limits = c(0,5000))+
#  coord_polar()+
#  theme_light()

# Month
#ggplot()+
#  geom_jitter(data = ped_explore, aes(x = Month, y = Hourly_Counts), width = 0.1)+
#  scale_y_continuous(limits = c(0,5000))+
#  coord_polar()+
#  theme_light()


# create data
time <- as.numeric(rep(seq(1,7),each=7))  # x Axis
value <- runif(49, 10, 100)               # y Axis
group <- rep(LETTERS[1:7],times=7)        # group, one shape per group
data <- data.frame(time, value, group)

# Almost identical to coord_polar() but lines are drawn straight regardless of polar coordinates
coord_straightpolar <- function(theta = 'x', start = 0, direction = 1, clip = "on") {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto(NULL, CoordPolar, theta = theta, r = r, start = start,
          direction = sign(direction), clip = clip,
          # This is the different bit
          is_linear = function(){TRUE})
}



# stacked area chart
ggplot(data, aes(x=time, y=value, fill=group)) + 
  geom_area()+
  coord_straightpolar()


# explore hourly counts
par(mfrow = c(1, 1))
hist(melb_data$Hourly_Counts, breaks = 1000)


mean(melb_data$Hourly_Counts)

var(melb_data$Hourly_Counts)


#plot sensors with adaquate data faceted by sensor_ID
ggplot()+
  geom_point(data = melb_data, aes(x = Date_Time, y = Hourly_Counts))+
  facet_wrap(~Sensor_ID)

install.packages("plotly")
install.packages("htmlwidgets")
library(plotly)
library(htmlwidgets)

# plotting predicted data

all_predictions <- read.csv("processed_data/predictions_for_all_sensors.csv", header=TRUE)

# find out basic info about each sensor
sensor_predict_info <- all_predictions %>%
  group_by(sensor_id) %>%
  summarise(n = n(), avg = mean(fit), max = max(fit)) %>%
  arrange(desc(max))

#read in sensor location data
senloc <- read.csv("https://www.learnatschoolschool.com/wp-content/uploads/2020/01/Pedestrian_Counting_System_-_Sensor_Locations.csv",header=TRUE)


all <- merge(all_predictions, dplyr::select(senloc, sensor_id, sensor_description), by = "sensor_id")

# find the max
max(all_predictions$fit)

# order Day of the week factor
all$Day <- factor(all$Day,
                          levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                          ordered = TRUE)

# order Time factor
all$Time <- factor(all$Time,
                           levels = as.character(c(0:23)),
                           ordered = TRUE)

# time as a number
all$Time <- as.numeric(all$Time)

# predictions rounded to a count
all$fit <- round(all$fit, 0)
all$upr <- round(all$upr, 0)
all$lwr <- round(all$lwr, 0)

# change column names for plot
colnames(all) <- c("sensor_id", "X", "Day", "Time", "Predicted_Pedestrians", "lwr", "upr", "Location")



#Plot Time
ggplot(data = all[which(all$Day == "Monday"),])+
  #geom_ribbon(aes(x = Time, ymin = mean(lwr), ymax = mean(upr), fill = "grey70")) +
  geom_boxplot(aes(x = Time, y = Predicted_Pedestrians, group = Time))



#Plot Time as a line for each sensor
time_plot <- ggplot(data = all[which(all$Day == "Monday"),])+
  geom_segment(aes(x = 7 , y = 0, xend = 7, yend = 10000), size = 3, color = "#76ee00")+
  geom_line(aes(y = Predicted_Pedestrians, x = Time, group = Location), color = "grey60") +
  geom_smooth(aes(y = Predicted_Pedestrians, x = Time), color = "#308014", size = 1.5) +
  scale_x_continuous(limits = c(0,23), expand = c(0,0), breaks = c(0:23)) +
  scale_y_continuous(limits = c(0, 10000), expand = c(0,0)) +
  theme_light()

time_plot

ggplotly(time_plot, tooltip = c("Location", "y"))


#Plot Day as a line for each sensor
day_plot <- ggplot(data = all[which(all$Time == "13"),], aes(y = Predicted_Pedestrians, x = Day))+
  geom_segment(data = data.frame(c()), aes(x = "Monday" , y = 0, xend = "Monday", yend = 5000), size = 27, color = "#76ee00", alpha = 0.5)+
  geom_boxplot(color = "grey60") +
  #geom_boxplot(data = all[which(all$Day == "Monday" & all$Time == "13"),], aes(y = Predicted_Pedestrians, x = Day), color = "#76ee00") +
  scale_y_continuous(limits = c(0, 5000), expand = c(0,0)) +
  theme_light()

day_plot

ggplotly(day_plot)


# follow the github tute to stop the factor order from changing
scael_x_discrete(drop = FALSE)



