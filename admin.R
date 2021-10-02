library(tidyverse)

count_df = read_csv('https://www.learnatschoolschool.com/count_data_for_shiny/')

write_csv(df)


loc_df = read_csv('https://www.learnatschoolschool.com/pedestrian_counting_system_-_sensor_locations/')

write_csv(loc_df, 'processed_data/sensor_locations.csv')

library(shiny)
library(rsconnect)

deployApp()

install.packages('Hmisc')
install.packages('Formula')
install.packages('MXM')
install.packages('MatrixModels')
install.packages("abind")
install.packages("car")
install.packages("carData")
install.packages("openxlsx")
install.packages("rio")
install.packages("zip")

install.packages('shiny')
install.packages('leaflet')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('plotly')
install.packages('BBmisc')

install.packages('htmltools')
install.packages('shinycssloaders')
install.packages('shinyWidgets')


