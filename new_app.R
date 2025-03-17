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
                        plotOutput("temperaturePlot"),
                        verbatimTextOutput("temperatureSummary")
                 )
               ),
               fluidRow(
                 column(12, 
                        h3("Precipitation Analysis"),
                        plotOutput("precipitationPlot"),
                        verbatimTextOutput("precipitationSummary")
                 )
               ),
               fluidRow(
                 column(12, 
                        h3("Wind Analysis"),
                        plotOutput("windPlot"),
                        verbatimTextOutput("windSummary")
                 )
               ),
               fluidRow(
                 column(12, 
                        h3("Wind Direction Analysis"),
                        plotOutput("windRosePlot"),
                        verbatimTextOutput("windRoseSummary")
                 )
               ),
               fluidRow(
                 column(12, 
                        h3("Sunshine Analysis"),
                        plotOutput("sunlightPlot"),
                        verbatimTextOutput("sunlightSummary")
                 )
               ),
               fluidRow(
                 column(12, 
                        h3("Radiaation/Evapotranspiration Analysis"),
                        plotOutput("radiationEvapPlot"),
                        verbatimTextOutput("radiationEvapSummary")
                 )
               ),
      ),
      
      tabPanel("Air Quality Analysis",
               br(),
               selectInput("airNormMethod", "Normalization Method:",
                           choices = c("None" = "none", 
                                       "Z-Score (scale)" = "zscore", 
                                       "Min-Max (0-1)" = "minmax"),
                           selected = "none"),
               plotOutput("airPlot", height = "500px"),
               br(),
               h4("Air Quality Data Summary"),
               verbatimTextOutput("airSummary")
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
    air_vars <- "pm10,carbon_monoxide,carbon_dioxide,sulphur_dioxide,dust"
    tz <- input$timezone
    
    if (as.numeric(substr(start_date, 1, 4)) < 2023) {
      start_date <- "2022-07-01"
    }
  
    paste0(
      "https://air-quality-api.open-meteo.com/v1/air-quality",
      "?latitude=", lat,
      "&longitude=", lon,
      "&start_date=", start_date,
      "&end_date=", end_date,
      "&hourly=", air_vars,
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
  
  air_df <- reactive({
    req(airquality_data())
    data <- airquality_data()
    df <- data.frame(data$hourly)
    df$time <- as.Date(df$time)

    df %>% drop_na()
    
    if(input$airNormMethod != "none" && ncol(df) > 1) {
      numeric_cols <- setdiff(names(df), "time")
      if(input$airNormMethod == "zscore") {
        df[numeric_cols] <- scale(df[numeric_cols])
      } else if(input$airNormMethod == "minmax") {
        min_max_normalize <- function(x) {
          (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
        }
        df[numeric_cols] <- lapply(df[numeric_cols], min_max_normalize)
      }
        
    }
    return (df)
    
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
  
  output$sunlightPlot <- renderPlot({
    req(climate_df())
    df <- climate_df()
    
    sunlight_data <- df %>%
      select(time, daylight_duration, sunshine_duration) %>%
      mutate(
        daylight_hours = daylight_duration / 3600,
        sunshine_hours = sunshine_duration / 3600
      ) %>%
      select(time, daylight_hours, sunshine_hours) %>%
      pivot_longer(
        cols = c("daylight_hours", "sunshine_hours"),
        names_to = "type",
        values_to = "hours"
      ) %>%
      mutate(type = factor(type, 
                           levels = c("daylight_hours", "sunshine_hours"),
                           labels = c("Daylight", "Sunshine")))
    
    ggplot(sunlight_data, aes(x = time, y = hours, color = type)) +
      geom_line(linewidth = 0.8) +
      scale_color_manual(values = c("Daylight" = "#FFA500", "Sunshine" = "#FFD700")) +
      labs(
        title = "Daylight and Sunshine Duration",
        x = "Date",
        y = "Hours",
        color = ""
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$radiationEvapPlot <- renderPlot({
    req(climate_df())
    df <- climate_df()
    
    # Create plot with two y-axes
    par(mar = c(5, 5, 4, 5))
    
    # Plot radiation
    plot(df$time, df$shortwave_radiation_sum, 
         type = "l", col = "orange", lwd = 2,
         xlab = "Date", ylab = "Shortwave Radiation (MJ/m²)",
         main = "Radiation and Evapotranspiration")
    
    # Add second y-axis for evapotranspiration
    par(new = TRUE)
    plot(df$time, df$et0_fao_evapotranspiration, 
         type = "l", col = "blue", lwd = 2,
         xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    axis(side = 4)
    mtext("Evapotranspiration (mm)", side = 4, line = 3)
    
    # Add legend
    legend("topleft", 
           legend = c("Shortwave Radiation", "Evapotranspiration"),
           col = c("orange", "blue"), 
           lty = 1, lwd = 2)
  })
  
  output$airPlot <- renderPlot({
    req(air_df())
    df <- air_df()
    
    # Prepare data for plotting
    plot_df <- df %>%
      pivot_longer(cols = -time, names_to = "pollutant", values_to = "concentration")
    
    # Create the plot
    ggplot(plot_df, aes(x = time, y = concentration, color = pollutant)) +
      geom_line() +
      geom_point() +
      labs(
        title = "Air Pollutant Concentrations Over Time",
        x = "Date",
        y = "Concentration",
        color = "Pollutant"
      ) +
      theme_minimal() +
      theme(
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$temperatureSummary <- renderPrint({
    req(climate_df())
    df <- climate_df()
    
    temp_summary <- df %>%
      summarise(
        avg_max = mean(temperature_2m_max, na.rm = TRUE),
        avg_min = mean(temperature_2m_min, na.rm = TRUE),
        avg_mean = mean(temperature_2m_mean, na.rm = TRUE),
        max_temp = max(temperature_2m_max, na.rm = TRUE),
        min_temp = min(temperature_2m_min, na.rm = TRUE)
      )
    
    print(temp_summary)
  })
  
  output$precipitationSummary <- renderPrint({
    req(climate_df())
    df <- climate_df()
    
    precip_summary <- df %>%
      summarise(
        total_precip = sum(precipitation_sum, na.rm = TRUE),
        avg_daily_precip = mean(precipitation_sum, na.rm = TRUE),
        max_daily_precip = max(precipitation_sum, na.rm = TRUE),
        total_rain = sum(rain_sum, na.rm = TRUE),
        total_snow = sum(snowfall_sum, na.rm = TRUE)
      )
    
    print(precip_summary)
  })
  
  output$windSummary <- renderPrint({
    req(climate_df())
    df <- climate_df()
    
    wind_summary <- df %>%
      summarise(
        avg_wind_speed = mean(wind_speed_10m_max, na.rm = TRUE),
        max_wind_speed = max(wind_speed_10m_max, na.rm = TRUE),
        avg_wind_gusts = mean(wind_gusts_10m_max, na.rm = TRUE),
        max_wind_gusts = max(wind_gusts_10m_max, na.rm = TRUE)
      )
    
    print(wind_summary)
  })
  
  output$windDirectionSummary <- renderPrint({
    req(climate_df())
    df <- climate_df()
    
    # Create wind direction categories
    direction_labels <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
    df$direction_category <- cut(df$wind_direction_10m_dominant, 
                                 breaks = seq(0, 360, by = 45), 
                                 labels = direction_labels,
                                 include.lowest = TRUE)
    
    wind_dir_summary <- df %>%
      group_by(direction_category) %>%
      summarise(
        count = n(),
        percent = n() / nrow(df) * 100,
        avg_speed = mean(wind_speed_10m_max, na.rm = TRUE)
      ) %>%
      arrange(desc(count))
    
    print(wind_dir_summary)
  })
  
  output$sunlightSummary <- renderPrint({
    req(climate_df())
    df <- climate_df()
    
    sunlight_summary <- df %>%
      summarise(
        avg_daylight = mean(daylight_duration/3600, na.rm = TRUE),
        max_daylight = max(daylight_duration/3600, na.rm = TRUE),
        min_daylight = min(daylight_duration/3600, na.rm = TRUE),
        avg_sunshine = mean(sunshine_duration/3600, na.rm = TRUE),
        max_sunshine = max(sunshine_duration/3600, na.rm = TRUE),
        min_sunshine = min(sunshine_duration/3600, na.rm = TRUE)
      )
    
    print(sunlight_summary)
  })
  
  output$radiationEvapSummary <- renderPrint({
    req(climate_df())
    df <- climate_df()
    
    radiation_evap_summary <- df %>%
      summarise(
        avg_radiation = mean(shortwave_radiation_sum, na.rm = TRUE),
        max_radiation = max(shortwave_radiation_sum, na.rm = TRUE),
        min_radiation = min(shortwave_radiation_sum, na.rm = TRUE),
        avg_evapotranspiration = mean(et0_fao_evapotranspiration, na.rm = TRUE),
        max_evapotranspiration = max(et0_fao_evapotranspiration, na.rm = TRUE),
        min_evapotranspiration = min(et0_fao_evapotranspiration, na.rm = TRUE),
        total_evapotranspiration = sum(et0_fao_evapotranspiration, na.rm = TRUE)
      )
    
    print(radiation_evap_summary)
  })
  
  output$airSummary <- renderPrint({
    req(air_df())
    df <- air_df()
    
    # Calculate summary statistics for pollutants
    pollutant_summary <- df %>%
      summarise(
        avg_carbon_monoxide = mean(carbon_monoxide, na.rm = TRUE),
        max_carbon_monoxide = max(carbon_monoxide, na.rm = TRUE),
        min_carbon_monoxide = min(carbon_monoxide, na.rm = TRUE),
        
        avg_carbon_dioxide = mean(carbon_dioxide, na.rm = TRUE),
        max_carbon_dioxide = max(carbon_dioxide, na.rm = TRUE),
        min_carbon_dioxide = min(carbon_dioxide, na.rm = TRUE),
        
        avg_sulphur_dioxide = mean(sulphur_dioxide, na.rm = TRUE),
        max_sulphur_dioxide = max(sulphur_dioxide, na.rm = TRUE),
        min_sulphur_dioxide = min(sulphur_dioxide, na.rm = TRUE),

        avg_pm10 = mean(pm10, na.rm = TRUE),
        max_pm10 = max(pm10, na.rm = TRUE),
        min_pm10 = min(pm10, na.rm = TRUE),
        
        avg_dust = mean(dust, na.rm = TRUE),
        max_dust = max(dust, na.rm = TRUE),
        min_dust = min(dust, na.rm = TRUE),
      )
    
    print(pollutant_summary)
  })
  
}

shinyApp(ui = ui, server = server)
