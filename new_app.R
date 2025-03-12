library(httr)
library(jsonlite)
library(ggplot2)
library(tidyr)
library(dplyr)
library(shiny)
library(leaflet)

get_data <- function(url) {
  encoded_url <- URLencode(url)
  response <- GET(encoded_url)
  
  if (http_status(response)$category == "Success") {
    json_data <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(json_data, flatten = TRUE)
    print(names(data))
  } else {
    print(paste("API request failed with status code:", http_status(response)$status_code))
    print(http_status(response)$reason)
  }
  
  return(data)
}

ui <- fluidPage(
  mainPanel(
    tabsetPanel(
      tabPanel("Overview", 
               h4("Location Settings"),
               leafletOutput("inputMap", height = "300px"),
               verbatimTextOutput("selectedLocation"),
               
               h4("Time Period"),
               dateRangeInput("dateRange", "Date Range:",
                              start = "2020-01-01",
                              end = "2023-01-01"),
               
               br(),
               h4("API URLs"),
               verbatimTextOutput("climateUrl"),
               verbatimTextOutput("airQualityUrl"),
               hr(),
               h4("Data Status"),
               verbatimTextOutput("dataStatus"),
               
               h4("Additional Settings"),
               selectInput("tempUnit", "Temperature Unit:",
                           choices = c("Celsius" = "celsius", "Fahrenheit" = "fahrenheit"),
                           selected = "celsius"),
               selectInput("timezone", "Timezone:", 
                           choices = c("Auto" = "auto", "GMT" = "GMT", "UTC" = "UTC"),
                           selected = "GMT"),
               
               br(),
               actionButton("updateBtn", "Fetch Data", class = "btn-primary", width = "100%"),
               br(), br()
      ),
      
      tabPanel("Climate Analysis",
               fluidRow(
                 column(12, 
                        h3("Temperature Analysis"),
                        plotOutput("temperaturePlot")
                 )
               ),
               fluidRow(
                 column(12, 
                        h3("Precipitation Analysis"),
                        plotOutput("precipitationPlot")
                 )
               ),
               fluidRow(
                 column(12, 
                        h3("Wind Analysis"),
                        plotOutput("windPlot")
                 )
               ),
               fluidRow(
                 column(12, 
                        h3("Wind Direction Analysis"),
                        plotOutput("windRosePlot")
                 )
               ),
      )
    )
  )
)
  
   

server <- function(input, output, session) {
  climate_data <- reactiveVal(NULL)
  airquality_data <- reactiveVal(NULL)
  selected_location <- reactiveVal(c(23, 25))
  
  # Handle url generation
  climate_url <- reactive({
    lat <- selected_location()[1]
    lon <- selected_location()[2]
    start_date <- format(input$dateRange[1], "%Y-%m-%d")
    end_date <- format(input$dateRange[2], "%Y-%m-%d")
    temp_unit <- input$tempUnit
    tz <- input$timezone
    climate_vars <- "daylight_duration,sunshine_duration,precipitation_sum,rain_sum,snowfall_sum,wind_speed_10m_max,wind_gusts_10m_max,wind_direction_10m_dominant,shortwave_radiation_sum,et0_fao_evapotranspiration,temperature_2m_max,temperature_2m_min,temperature_2m_mean"
    
    paste0(
      "https://archive-api.open-meteo.com/v1/archive",
      "?latitude=", lat,
      "&longitude=", lon,
      "&start_date=", start_date,
      "&end_date=", end_date,
      "&daily=", climate_vars,
      "&timezone=", tz,
      "&temperature_unit=", temp_unit
    )
  })
  airquality_url <- reactive({
    lat <- selected_location()[1]
    lon <- selected_location()[2]
    start_date <- format(input$dateRange[1], "%Y-%m-%d")
    end_date <- format(input$dateRange[2], "%Y-%m-%d")
    hourly_vars <- paste(input$airVariables, collapse = ",")
    tz <- input$timezone
    
    if (as.numeric(substr(start_date, 1, 4)) < 2014) {
      start_date <- "2014-01-01"
    }
  
    paste0(
      "https://air-quality-api.open-meteo.com/v1/air-quality",
      "?latitude=", lat,
      "&longitude=", lon,
      "&start_date=", start_date,
      "&end_date=", end_date,
      "&hourly=", hourly_vars,
      "&timezone=", tz,
      "&domains=cams_global"
    )
  })
  output$climateUrl <- renderText({
    paste("Climate API URL:", climate_url())
  })
  output$airQualityUrl <- renderText({
    paste("Air Quality API URL:", airquality_url())
  })
  
  # Handle fetching 
  observeEvent(input$updateBtn, {
    withProgress(message = 'Fetching data...', {
      
      setProgress(0.2, detail = "Fetching climate data...")
      climate_data(get_data(climate_url()))
      setProgress(0.6, detail = "Fetching air quality data...")
      airquality_data(get_data(airquality_url()))
      
      setProgress(1, detail = "Complete!")
    })
  })
  output$dataStatus <- renderText({
    if(is.null(climate_data()) && is.null(airquality_data())) {
      return("No data fetched yet. Click 'Fetch Data' to retrieve data.")
    }
    
    climate_status <- if(!is.null(climate_data())) "Successfully fetched" else "Failed to fetch"
    air_status <- if(!is.null(airquality_data())) "Successfully fetched" else "Failed to fetch"
    
    paste("Climate Data:", climate_status, "\nAir Quality Data:", air_status)
  })
  
  # Make the data frames
  climate_df <- reactive({
    req(climate_data())
    data <- climate_data()
    
    df <- data.frame(data$daily)
    df$time <- as.Date(df$time)
    print(df)
    
    return(df)
  })
  
  ## MAP
  output$inputMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = selected_location()[2], lat = selected_location()[1], 
                 layerId = "selectedMarker") %>%
      setView(lng = selected_location()[2], lat = selected_location()[1], zoom = 2)
  })
  observeEvent(input$inputMap_click, {
    click <- input$inputMap_click
    selected_location(c(click$lat, click$lng))
    
    # Update marker position
    leafletProxy("inputMap") %>%
      clearMarkers() %>%
      addMarkers(lng = click$lng, lat = click$lat, layerId = "selectedMarker")
  })
  output$selectedLocation <- renderText({
    paste("Selected Location - Latitude:", round(selected_location()[1], 4), 
          "Longitude:", round(selected_location()[2], 4))
  })
  
  ## PLOTS
  output$temperaturePlot <- renderPlot({
    req(climate_df())
    df <- climate_df()
    temp_data <- df %>%
      select(time, temperature_2m_max, temperature_2m_min, temperature_2m_mean) %>%
      pivot_longer(
        cols = c("temperature_2m_max", "temperature_2m_min", "temperature_2m_mean"),
        names_to = "measurement",
        values_to = "temperature"
      ) %>%
      mutate(measurement = factor(measurement, 
                                  levels = c("temperature_2m_max", "temperature_2m_mean", "temperature_2m_min"),
                                  labels = c("Maximum", "Mean", "Minimum")))
    
    # Create the temperature plot
    ggplot(temp_data, aes(x = time, y = temperature, color = measurement)) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 1) +
      scale_color_manual(values = c("Maximum" = "#FF5733", "Mean" = "#33A8FF", "Minimum" = "#33FF57")) +
      labs(
        title = "Temperature Trends Over Time",
        x = "Date",
        y = paste("Temperature (", ifelse(input$tempUnit == "celsius", "°C", "°F"), ")", sep = ""),
        color = "Measurement"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
  })
  
  output$precipitationPlot <- renderPlot({
    req(climate_df())
    df <- climate_df()
    
    precip_data <- df %>%
      select(time, precipitation_sum, rain_sum, snowfall_sum) %>%
      pivot_longer(
        cols = c("precipitation_sum", "rain_sum", "snowfall_sum"),
        names_to = "type",
        values_to = "amount"
      ) %>%
      mutate(type = factor(type, 
                           levels = c("precipitation_sum", "rain_sum", "snowfall_sum"),
                           labels = c("Total Precipitation", "Rain", "Snowfall")))
    
    ggplot(precip_data, aes(x = time, y = amount, fill = type)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_brewer(palette = "Blues") +
      labs(
        title = "Precipitation Analysis",
        x = "Date",
        y = "Amount (mm)",
        fill = "Type"
      ) +
      theme_minimal() + 
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$windPlot <- renderPlot({
    req(climate_df())
    df <- climate_df()
    
    wind_data <- df %>%
      select(time, wind_speed_10m_max, wind_gusts_10m_max) %>%
      pivot_longer(
        cols = c("wind_speed_10m_max", "wind_gusts_10m_max"),
        names_to = "measurement",
        values_to = "speed"
      ) %>%
      mutate(measurement = factor(measurement, 
                                  levels = c("wind_speed_10m_max", "wind_gusts_10m_max"),
                                  labels = c("Max Wind Speed", "Max Wind Gusts")))
    
    ggplot(wind_data, aes(x = time, y = speed, color = measurement)) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 1) +
      scale_color_manual(values = c("Max Wind Speed" = "#3366CC", "Max Wind Gusts" = "#FF9900")) +
      labs(
        title = "Wind Speed Analysis",
        x = "Date",
        y = "Speed (km/h)",
        color = "Measurement"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$windRosePlot <- renderPlot({
    req(climate_df())
    df <- climate_df()
    
    # Create wind direction categories
    direction_labels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
    df$direction_category <- cut(df$wind_direction_10m_dominant, 
                                 breaks = seq(0, 360, by = 45), 
                                 labels = direction_labels,
                                 include.lowest = TRUE)
    
    # Count occurrences of each direction
    wind_dir_counts <- df %>%
      group_by(direction_category) %>%
      summarise(count = n(), 
                avg_speed = mean(wind_speed_10m_max, na.rm = TRUE)) %>%
      ungroup()
    
    # Convert to polar coordinates for wind rose
    ggplot(wind_dir_counts, aes(x = direction_category, y = count, fill = avg_speed)) +
      geom_bar(stat = "identity", width = 0.9) +
      coord_polar() +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(
        title = "Wind Direction Distribution",
        fill = "Avg. Max Speed (km/h)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_blank(),
        legend.position = "bottom"
      )
  })
  
}

shinyApp(ui = ui, server = server)
