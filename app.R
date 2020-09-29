library(shiny)
library(leaflet)
#library(rgeos)
library(htmltools)
library(dplyr)
library(ggplot2)
#library(deldir)
#library(memisc)
library(BBmisc)
library(plotly)
#library(markdown)
library(knitr)

# colour scheme
# sap green #308014
# chartreuse #76ee00


#read in sensor location data
senloc <- read.csv("https://www.learnatschoolschool.com/wp-content/uploads/2020/01/Pedestrian_Counting_System_-_Sensor_Locations.csv",header=TRUE)

# add a single column to the sensor location data for Direction
senloc <- senloc %>%
    filter(!direction_1 == "") %>%
    mutate(direction = ifelse(direction_1 %in% c("North", "South"), "NS", "EW"))


# read in predicted values
all_predictions <- read.csv("processed_data/predictions_for_all_sensors.csv", header=TRUE)

# merge predicted values with the sensors description
all <- merge(all_predictions, dplyr::select(senloc, sensor_id, sensor_description), by = "sensor_id")

# find the max
max(all_predictions$fit)

# order Day of the week factor
all$Day <- factor(all$Day,
                  levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                  ordered = TRUE)

# time as a number
all$Time <- as.numeric(all$Time)

# predictions rounded to a count
all$fit <- round(all$fit, 0)
all$upr <- round(all$upr, 0)
all$lwr <- round(all$lwr, 0)

# change column names for plot
colnames(all) <- c("sensor_id", "X", "Day", "Time", "Predicted_Pedestrians", "lwr", "upr", "Location")


ui <- fluidPage(
    
    titlePanel("Melbourne's Foot Traffic during 2019 - By Month, Day, and Hour"),
    
    
    fluidRow(
               column(6,
            

            selectInput(inputId = "day", "Day:",
                        c("Monday" = "Monday",
                          "Tuesday" = "Tuesday",
                          "Wednesday"= "Wednesday",
                          "Thursday" = "Thursday",
                          "Friday" = "Friday",
                          "Saturday" = "Saturday",
                          "Sunday" = "Sunday")),
            
            plotlyOutput("day_plot")
               ),
        column(6, 
            
            
            sliderInput(inputId = "hour", "Hour of the Day (24hr time):",
                        min = 0, max = 23,
                        value = 12),
            radioButtons(inputId = "covid", "COVID?", c("Yes" = 1, "No" = 0)),
            plotlyOutput("time_plot"),
            
            p("Select Month, Day, and Hour filters, then click on the polygons to see the average number of pedestrians."),
        )),
    fluidRow(
        column(12, 
            
            leafletOutput("mymap"),
            p("An interactive map exploring City of Melbourne's variation in foot traffic. Darker icons = More pedestrians")
            
        )#,
        #uiOutput('markdown')
        ))



server <- function(input, output, session) {
    
    
    selected <- reactive({
        sensor_data_frame <- all_predictions %>% 
            mutate(busy = normalize(fit, method = "range", range = c(0.15, 1))) %>%
            filter(Day %in% input$day & Time == input$hour & Covid == input$covid)
        sensor_data_frame
    })
    
    # Make a list of icons. We'll index into it based on name.
    sensor_icons <- iconList(
        EW = makeIcon("images/icon_ew.png", 
                      iconWidth = 38, 
                      iconHeight = 38,
                      iconAnchorX = 20,
                      iconAnchorY = 20),
        NS = makeIcon("images/icon_ns.png",
                      iconWidth = 38, 
                      iconHeight = 38,
                      iconAnchorX = 20,
                      iconAnchorY = 20)
    )

    output$mymap <- renderLeaflet({
        leaflet() %>%
            setView(lng = 144.9631, lat = -37.8136, zoom = 14) %>%
            setMaxBounds(144.9331, -37.7836, 144.9931, -37.8436) %>%
            addProviderTiles(providers$CartoDB.Positron)
    })
    
    
    #output$day_plot <- renderPlot(
    #    ggplot()+
    #        geom_boxplot(data = all_predictions, aes(x = Day, y = predictions))#+
    #        #geom_boxplot(data = selected_day(), 
    #)
    
    output$time_plot <- renderPlotly({
        
        #Plot Time as a line for each sensor
        ggplotly(
            ggplot()+
                #geom_segment(aes(x = 7 , y = 0, xend = 7, yend = 6000), size = 5, color = "#76ee00", alpha = 0.2)+
                geom_line(data = all[which(all$Day == input$day),], aes(y = Predicted_Pedestrians, x = Time, group = Location), color = "grey60") +
                geom_smooth(data = all[which(all$Day == input$day),], aes(y = Predicted_Pedestrians, x = Time), color = "#308014", size = 1.5) +
                scale_x_continuous(limits = c(0,23), expand = c(0,0), breaks = c(0:23)) +
                scale_y_continuous(limits = c(0, 5500), expand = c(0,0)) +
                theme_light()
            , tooltip = c("group", "y"))
        
    })
    
    output$day_plot<- renderPlotly({
        
        #Plot Day as a boxplot for each day
        ggplotly(
            ggplot(data = all[which(all$Time == input$hour),], aes(y = Predicted_Pedestrians, x = Day))+
                geom_boxplot(color = "grey20", fill = "white") +
                #annotate('segment', x = "Monday", y = 0, xend = "Monday", yend = 6000, size = 20, color = "#76ee00", alpha = 0.2)+
                #geom_boxplot(data = all[which(all$Day == input$day & all$Time == input$hour),], aes(y = Predicted_Pedestrians, x = Day), color = "grey10", fill = "white") +
                scale_y_continuous(limits = c(0, 5500), expand = c(0,0)) +
                #scale_x_discrete(drop = FALSE) +
                theme_light()
        )
        
    })
   
    
    observe({
        if(!is.null(input$hour)){
            all_sensor_data <- merge(senloc, selected(), by="sensor_id", all.y = TRUE)
            leafletProxy("mymap", data = all_sensor_data) %>%
                clearMarkers() %>% 
                addMarkers(data = all_sensor_data,
                           lng = ~longitude,
                           lat = ~latitude,
                           icon = ~sensor_icons[direction],
                           options = markerOptions(opacity = all_sensor_data$busy),
                           popup = sprintf("Pedestrians per hour: %s",
                                          as.character(round(all_sensor_data$fit, 0))))
        }})
    
    output$markdown <- renderUI({
        HTML(markdown::markdownToHTML(knit('Walking_Weather.rmd', quiet = TRUE)))
    })
}

shinyApp(ui, server)


