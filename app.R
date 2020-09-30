# load packages
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)
library(BBmisc)

library(htmltools)
library(shinycssloaders)
library(shinyWidgets)

#library(remotes)

#install_version("conquer", version = "1.0.1", repos = "http://cran.us.r-project.org")

library(conquer)

### what to add next
#some fun facts: busiest day ever, busiest sensor

### colour scheme
# sap green #308014
# chartreuse #76ee00
# success material-ui green #5cb85c


#read in sensor location data
senloc <-
    read.csv(
        "https://www.learnatschoolschool.com/wp-content/uploads/2020/01/Pedestrian_Counting_System_-_Sensor_Locations.csv",
        header = TRUE,
        stringsAsFactors = FALSE
    )

# add a single column to the sensor location data for Direction
senloc <- senloc %>%
    filter(!direction_1 == "") %>%
    mutate(direction = ifelse(direction_1 %in% c("North", "South"), "NS", "EW"))


# read in predicted values
all_predictions <-
    read.csv("processed_data/predictions_for_all_sensors.csv",
             header = TRUE)

# merge predicted values with the sensors description
all <-
    merge(all_predictions,
          dplyr::select(senloc, sensor_id, sensor_description),
          by = "sensor_id")

# find the max
max_ped <- max(all_predictions$fit)

# order Day of the week factor
all$Day <- factor(
    all$Day,
    levels = c(
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
        "Sunday"
    ),
    ordered = TRUE
)

# time as a number
all$Time <- as.numeric(all$Time)

# predictions rounded to a count
all$fit <- round(all$fit, 0)
all$upr <- round(all$upr, 0)
all$lwr <- round(all$lwr, 0)

# change column names for plot
colnames(all) <-
    c(
        "sensor_id",
        "X",
        "Day",
        "Time",
        "Covid",
        "Predicted_Pedestrians",
        "lwr",
        "upr",
        "Location"
    )

# createa list of all the times of the day as strings
time_of_day <- c(
    "12 midnight",
    "1 am",
    "2 am",
    "3 am",
    "4 am",
    "5 am",
    "6 am",
    "7 am",
    "8 am",
    "9 am",
    "10 am",
    "11 am",
    "12 noon",
    "1 pm",
    "2 pm",
    "3 pm",
    "4 pm",
    "5 pm",
    "6 pm",
    "7 pm",
    "8 pm",
    "9 pm",
    "10 pm",
    "11 pm"
)

ui <- fluidPage(tags$style(
    HTML(
        ".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #5cb85c}"
    )
),

fluidRow(
    column(
        10,
        offset = 1,
        
        a(name = "top"),
        
        titlePanel("Explore Melbourne's Foot Traffic - One step at a time!"),
        br(),
        
        fluidRow(
            column(
                4,
                offset = 2,
                align = "left",
                span(
                    "1. Firstly, select which day of the week you would like to analyse.",
                    style = "color:black; font-size:1.2em"
                )
            ),
            column(4,
                   selectInput(
                       inputId = "day",
                       "Day:",
                       c(
                           "Monday" = "Monday",
                           "Tuesday" = "Tuesday",
                           "Wednesday" = "Wednesday",
                           "Thursday" = "Thursday",
                           "Friday" = "Friday",
                           "Saturday" = "Saturday",
                           "Sunday" = "Sunday"
                       )
                   ))
        ),
        hr(),
        
        fluidRow(
            column(
                4,
                offset = 2,
                align = "left",
                span(
                    "2. Then, decide which time of the day you would like to investigate.",
                    style = "color:black; font-size:1.2em"
                )
            ),
            column(
                4,
                sliderTextInput(
                    inputId = "hour",
                    label = "Time of Day",
                    grid = FALSE,
                    force_edges = TRUE,
                    selected = "12 noon",
                    choices = time_of_day
                )
            )
        ),
        hr(),
        fluidRow(
            column(
                4,
                offset = 2,
                align = "left",
                span(
                    "3. Lastly, choose whether to apply the effect of the COVID-19 lockdown",
                    style = "color:black; font-size:1.2em"
                )
            ),
            column(
                4,
                materialSwitch(
                    inputId = "covid",
                    label = "",
                    value = FALSE,
                    right = FALSE,
                    status =  "success"
                )
            )
        ),
        hr(),
        fluidRow(column(
            12,
            p("Foot traffic across the City of Melbourne, Australia.", style =
                  "color:black; font-size:1.4em"),
            leafletOutput("mymap", height = 400) %>%
                withSpinner(type = 6, color = "#5cb85c"),
            span(
                "Each icon represents a pedestrian sensor: Darker icons = More pedestrians. Click on an sensor icon to see the predicted number of pedestrians.",
                style = "font-size:0.9em;"
            ),
            br(),
            br()
        )),
        fluidRow(
            column(
                6,
                span(htmlOutput("time_text"), style = "color:black; font-size:1.4em"),
                plotlyOutput("time_plot") %>%
                    withSpinner(type = 6, color = "#5cb85c"),
                span(
                    "Each grey line represents a sensor's counts throughout the specified day. The green line represents the smoothed trend across all sensors. Hover over each line to see the predicted number of pedestrians & sensor info.",
                    style = "font-size:0.9em;"
                )
            ),
            column(
                6,
                span(htmlOutput("day_text"), style = "color:black; font-size:1.4em"),
                plotlyOutput("day_plot") %>%
                    withSpinner(type = 6, color = "#5cb85c"),
                span(
                    "Each box represents the distribution of Sensor counts for different days of the week at the specified time. Hover over each box to see the distribution's info.",
                    style = "font-size:0.9em;"
                )
            )
        ),
        
        fluidRow(
            br(),
            a(href = "#top", "^ Back to top"),
            hr(),
            h4("Explanation"),
            
            span(
                "The data you are looking at here has all been generated by a model, this model learnt these patterns from over 10 years worth of pedestrian data collected and published by the City of Melbourne."
            ),
            span(
                a(href = "https://data.melbourne.vic.gov.au/Transport/Pedestrian-Counting-System-2009-to-Present-counts-/b2ak-trbp", "Available here (Counts)")
            ),
            span("&"),
            span(
                a(href = "https://data.melbourne.vic.gov.au/Transport/Pedestrian-Counting-System-Sensor-Locations/h57g-5234", "here (Locations)")
            ),
            br(),
            br(),
            p(
                "The purpose of this dashboard is to explore the relationships between the number of pedestrians in Melbourne and the predictor variables: hour of the day, day of the week, and the covid-19 lockdown. Therefore the selection process for out model focused on a good fit & interpretablility, not predicton power."
            ),
            
            p(
                "After many tests, it was dedcided each sensor would get its own individual Negative Binomial regression model. This model is suited for counts, and includes a parameter to handle the large variability in the data"
            ),
            
            p(
                "Day of the week, and the covid-19 lockdown were included as dummy variables, hour of the day was included as a circular variable using the equation cos(pi*Time/12) + sin(pi*Time/12)."
            ),
            
            p(
                "The next thing to add would be info surrounding public holidays & events. The model is limited when it comes to predicting these large, seemingly random spikes in the data."
            ),
            
            br(),
            
            a(href = "https://github.com/alecsharpie/where-to-walk-v2", "Link to Github repository"),
            br(),
            br(),
            br(),
            br()
            
        )
        
    )
))



server <- function(input, output, session) {
    selected <- reactive({
        sensor_data_frame <- all_predictions %>%
            mutate(busy = normalize(fit, method = "range", range = c(0.1, 1))) %>%
            filter(Day %in% input$day &
                       Time == match(input$hour, time_of_day) &
                       Covid == input$covid)
        sensor_data_frame
    })
    
    # Make a list of icons. We'll index into it based on name.
    sensor_icons <- iconList(
        EW = makeIcon(
            "images/icon_ew.png",
            iconWidth = 38,
            iconHeight = 38,
            iconAnchorX = 20,
            iconAnchorY = 20
        ),
        NS = makeIcon(
            "images/icon_ns.png",
            iconWidth = 38,
            iconHeight = 38,
            iconAnchorX = 20,
            iconAnchorY = 20
        )
    )
    
    output$mymap <- renderLeaflet({
        leaflet(options = leafletOptions(minZoom = 12, maxZoom = 18)) %>%
            setView(lng = 144.9631,
                    lat = -37.8136,
                    zoom = 14) %>%
            setMaxBounds(144.9331, -37.7836, 144.9931, -37.8436) %>%
            addProviderTiles(providers$CartoDB.Positron)
    })
    
    
    output$time_text <-
        renderText({
            paste("Foot traffic over each Hour of the Day for <b>",
                  input$day,
                  "</b>")
        })
    
    
    output$time_plot <- renderPlotly({
        #Plot Time as a line for each sensor
        ggplotly(
            ggplot() +
                geom_line(
                    data = all[which(all$Day == input$day &
                                         all$Covid == input$covid),],
                    aes(
                        y = Predicted_Pedestrians,
                        x = Time,
                        group = Location
                    ),
                    color = "grey60"
                ) +
                geom_smooth(
                    data = all[which(all$Day == input$day &
                                         all$Covid == input$covid),],
                    aes(y = Predicted_Pedestrians, x = Time),
                    color = "#5cb85c",
                    size = 1.5
                ) +
                scale_x_continuous(
                    limits = c(0, 23),
                    expand = c(0, 0),
                    breaks = seq(3, 23, by = 3),
                    labels = function(x)
                        paste0(x, ":00")
                ) +
                scale_y_continuous(limits = c(0, (
                    max_ped + 200
                )), expand = c(0, 0)) +
                ylab("Number of Pedestrians") +
                xlab("Time of Day (24hr)") +
                theme_light()  +
                theme(axis.text.x = element_text(
                    angle = 30, vjust = 0.5
                ))
            ,
            tooltip = c("group", "y")
        )
        
    })
    
    output$day_text <-
        renderText({
            paste("Foot traffic across each Day of the Week at <b>",
                  input$hour,
                  "</b>")
        })
    
    output$day_plot <- renderPlotly({
        #Plot Day as a boxplot for each day
        ggplotly(
            ggplot(data = all[which(all$Time == match(input$hour, time_of_day) &
                                        all$Covid == input$covid),], aes(y = Predicted_Pedestrians, x = Day)) +
                geom_boxplot(color = "grey20", fill = "white") +
                scale_y_continuous(limits = c(0, (
                    max_ped + 200
                )), expand = c(0, 0)) +
                ylab("Number of Pedestrians") +
                xlab("Day of the week") +
                theme_light() +
                theme(axis.text.x = element_text(
                    angle = 30, vjust = 0.5
                ))
        )
        
    })
    
    
    observe({
        if (!is.null(input$hour)) {
            all_sensor_data <-
                merge(senloc,
                      selected(),
                      by = "sensor_id",
                      all.y = TRUE)
            leafletProxy("mymap", data = all_sensor_data) %>%
                clearMarkers() %>%
                addMarkers(
                    data = all_sensor_data,
                    lng = ~ longitude,
                    lat = ~ latitude,
                    icon = ~ sensor_icons[direction],
                    options = markerOptions(opacity = all_sensor_data$busy),
                    popup = sprintf("Pedestrians per hour: %s",
                                    as.character(
                                        round(all_sensor_data$fit, 0)
                                    ))
                )
        }
    })
}

shinyApp(ui, server)
