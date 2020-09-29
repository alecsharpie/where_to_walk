#read in weather data
#weather <- read.csv("data/melbourne_weather_2009-2020.csv", header = TRUE, stringsAsFactors = FALSE)

#merge weather with ped data for Sensor 3 Melbourne central

#preprocess data for merge
#melb_central <- ped_count %>%
#  mutate(Date_Time = mdy_hms(Date_Time))%>%
#  mutate(date = date(Date_Time))%>%
#  mutate(Time = as.character(Time))%>%
#  filter(Sensor_ID == 3)%>%
#  select(date, Day, Time, Hourly_Counts)

#weather %<>%
#  mutate(date = ymd(date))

#merge weather with ped data
#mc_ped <- merge(melb_central, weather, by = "date", all.x = TRUE)  


#weather %<>%
#  mutate(date = ymd(date))

#merge weather with ped data
#ped <- merge(melb_data, weather, by = "date", all.x = TRUE)  



# Millimeters of rain vs counts
ggplot()+
  geom_point(data = ped_model, aes(x = rain_mm, y = Hourly_Counts))+
  geom_smooth(data = ped_model, aes(x = rain_mm, y = Hourly_Counts))+
  scale_x_continuous(limits = c(0,25))

# Joules of solar energy vs counts
ggplot()+
  geom_point(data = ped_model, aes(x = sun_j, y = Hourly_Counts))+
  geom_smooth(data = ped_model, aes(x = sun_j, y = Hourly_Counts))

# Temperature vs counts
ggplot()+
  geom_point(data = ped_model, aes(x = temp_c, y = Hourly_Counts))+
  geom_smooth(data = ped_model, aes(x = temp_c, y = Hourly_Counts))


