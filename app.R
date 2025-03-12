library(httr)
library(jsonlite)
library(ggplot2)
library(tidyr)
library(dplyr)
library(shiny)
library(leaflet)

ui <- fluidPage(
  titlePanel("Climate and Air Quality Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Location Settings"),
      #numericInput("latitude", "Latitude:", value = 23, min = -90, max = 90, step = 0.1),
      #numericInput("longitude", "Longitude:", value = 25, min = -180, max = 180, step = 0.1),
      h4("Select Location on Map"),
      leafletOutput("inputMap", height = "300px"),
      verbatimTextOutput("selectedLocation"),
      
      
      h4("Time Period"),
      dateRangeInput("dateRange", "Date Range:",
                     start = "2020-01-01",
                     end = "2023-01-01"),
      
      # Temperature variables
      h4("Temperature Variables"),
      checkboxGroupInput("tempVariables", "Select Temperature Variables:",
                         choices = c("Maximum Temperature" = "temperature_2m_max", 
                                     "Minimum Temperature" = "temperature_2m_min",
                                     "Mean Temperature" = "temperature_2m_mean"),
                         selected = c("temperature_2m_max", "temperature_2m_min", "temperature_2m_mean")),
      
      # Air quality variables
      h4("Air Quality Variables"),
      checkboxGroupInput("airVariables", "Select Air Quality Variables:",
                         choices = c("Carbon Monoxide" = "carbon_monoxide", 
                                     "Carbon Dioxide" = "carbon_dioxide",
                                     "Nitrogen Dioxide" = "nitrogen_dioxide",
                                     "Sulphur Dioxide" = "sulphur_dioxide",
                                     "Methane" = "methane",
                                     "Ozone" = "ozone"),
                         selected = c("carbon_monoxide", "nitrogen_dioxide")),
      
      h4("Additional Settings"),
      selectInput("tempUnit", "Temperature Unit:",
                  choices = c("Celsius" = "celsius", "Fahrenheit" = "fahrenheit"),
                  selected = "celsius"),
      selectInput("timezone", "Timezone:", 
                  choices = c("Auto" = "auto", "GMT" = "GMT", "UTC" = "UTC"),
                  selected = "GMT"),
      
      br(),
      actionButton("updateBtn", "Fetch Data", class = "btn-primary", width = "100%")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Overview", 
                 br(),
                 h4("API URLs"),
                 verbatimTextOutput("climateUrl"),
                 verbatimTextOutput("airQualityUrl"),
                 hr(),
                 h4("Data Status"),
                 verbatimTextOutput("dataStatus")
        ),
        tabPanel("Temperature Analysis",
                 br(),
                 selectInput("tempView", "View Type:",
                             choices = c("Time Series", "Decomposition")),
                 plotOutput("tempPlot", height = "500px"),
                 br(),
                 h4("Temperature Data Summary"),
                 verbatimTextOutput("tempSummary")
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
)





server <- function(input, output, session) {

  climate_data <- reactiveVal(NULL)
  airquality_data <- reactiveVal(NULL)
  selected_location <- reactiveVal(c(23, 25))
  
  climate_url <- reactive({
    req(input$tempVariables)
    
    #lat <- input$latitude
    #lon <- input$longitude
    lat <- selected_location()[1]
    lon <- selected_location()[2]
    start_date <- format(input$dateRange[1], "%Y-%m-%d")
    end_date <- format(input$dateRange[2], "%Y-%m-%d")
    daily_vars <- paste(input$tempVariables, collapse = ",")
    temp_unit <- input$tempUnit
    
    paste0(
      "https://archive-api.open-meteo.com/v1/archive",
      "?latitude=", lat,
      "&longitude=", lon,
      "&start_date=", start_date,
      "&end_date=", end_date,
      "&daily=", daily_vars,
      "&temperature_unit=", temp_unit
    )
  })
  
  airquality_url <- reactive({
    req(input$airVariables)
    
    #lat <- input$latitude
    #lon <- input$longitude
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
  
  ###
  output$inputMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap tiles
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
  
  # Display selected coordinates
  output$selectedLocation <- renderText({
    paste("Selected Location - Latitude:", round(selected_location()[1], 4), 
          "Longitude:", round(selected_location()[2], 4))
  })
  ###
  
  # Display the URLs
  output$climateUrl <- renderText({
    paste("Climate API URL:", climate_url())
  })
  
  output$airQualityUrl <- renderText({
    paste("Air Quality API URL:", airquality_url())
  })
  
  # Fetch data when update button is clicked
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
  
  temp_df <- reactive({
    req(climate_data())
    data <- climate_data()
    
    if(!all(input$tempVariables %in% names(data$daily))) {
      return(NULL)
    }
    
    df <- data.frame(date = as.Date(data$daily$time))
    for(var in input$tempVariables) {
      if(var %in% names(data$daily)) {
        df[[var]] <- data$daily[[var]]
      }
    }
    
    return(df)
  })
  

  air_df <- reactive({
    req(airquality_data())
    data <- airquality_data()
    
    if(!"hourly" %in% names(data)) {
      return(NULL)
    }
    
    df <- data.frame(
      datetime = as.POSIXct(data$hourly$time, format="%Y-%m-%dT%H:%M")
    )
    
    for(var in input$airVariables) {
      if(var %in% names(data$hourly)) {
        df[[var]] <- data$hourly[[var]]
      }
    }
    
    df$date <- as.Date(df$datetime)
    
    daily_df <- df %>%
      group_by(date) %>%
      summarise(across(all_of(input$airVariables), mean, na.rm = TRUE))
    
    if(input$airNormMethod != "none" && ncol(daily_df) > 1) {
      numeric_cols <- setdiff(names(daily_df), "date")
      
      if(input$airNormMethod == "zscore") {
        daily_df[numeric_cols] <- scale(daily_df[numeric_cols])
      } else if(input$airNormMethod == "minmax") {
        min_max_normalize <- function(x) {
          (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
        }
        daily_df[numeric_cols] <- lapply(daily_df[numeric_cols], min_max_normalize)
      }
    }
    
    daily_df <- daily_df %>% drop_na()
    
    return(daily_df)
  })
  
  
  # Temperature plot
  output$tempPlot <- renderPlot({
    req(temp_df())
    df <- temp_df()
    
    if(input$tempView == "Time Series") {
      plot_df <- df %>%
        pivot_longer(cols = -date, names_to = "variable", values_to = "value")
      
      ggplot(plot_df, aes(x = date, y = value, color = variable)) +
        geom_line() +
        geom_point() +
        labs(
          title = "Temperature Variables Over Time",
          x = "Date",
          y = paste0("Value (", ifelse(input$tempUnit == "celsius", "°C", "°F"), ")"),
          color = "Variable"
        ) +
        theme_minimal() +
        theme(
          legend.position = "top",
          axis.text.x = element_text(angle = 45, hjust = 1)
        )
    } else {
      # Time series decomposition (using the first temperature variable)
      if(length(input$tempVariables) > 0) {
        var_to_decompose <- input$tempVariables[1]
        
        if(var_to_decompose %in% names(df)) {
          ts_data <- ts(df[[var_to_decompose]], frequency = 365)
          decomp_result <- tryCatch({
            stats::decompose(ts_data)
          }, error = function(e) {
            return(NULL)
          })
          
          if(!is.null(decomp_result)) {
            # Create dataframe for plotting
            decomp_df <- data.frame(
              date = df$date[1:length(decomp_result$trend)],
              observed = as.numeric(ts_data)[1:length(decomp_result$trend)],
              trend = as.numeric(decomp_result$trend),
              seasonal = as.numeric(decomp_result$seasonal),
              remainder = as.numeric(decomp_result$random)
            )
            
            decomp_long <- decomp_df %>%
              filter(!is.na(trend)) %>%
              pivot_longer(cols = c(observed, trend, seasonal, remainder),
                           names_to = "component", 
                           values_to = "value")
            
            ggplot(decomp_long, aes(x = date, y = value)) +
              geom_line(color = "steelblue") +
              facet_wrap(~ component, scales = "free_y", ncol = 1) +
              labs(
                title = paste("Decomposition of", var_to_decompose),
                x = "Date",
                y = paste0("Value (", ifelse(input$tempUnit == "celsius", "°C", "°F"), ")")
              ) +
              theme_minimal() +
              theme(
                strip.background = element_rect(fill = "lightgray"),
                strip.text = element_text(face = "bold")
              )
          } else {
            ggplot() + 
              annotate("text", x = 0.5, y = 0.5, 
                       label = "Cannot decompose time series. Try a different date range or variable.") + 
              theme_void()
          }
        } else {

          ggplot() + 
            annotate("text", x = 0.5, y = 0.5, 
                     label = "Selected variable not found in data.") + 
            theme_void()
        }
      } else {
        # Return error message plot
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                   label = "No temperature variables selected.") + 
          theme_void()
      }
    }
  })
  

  output$airPlot <- renderPlot({
    req(air_df())
    df <- air_df()
    
    if(nrow(df) == 0 || ncol(df) <= 1) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "No air quality data available for the selected parameters.") + 
               theme_void())
    }
    
    # Prepare data for plotting
    plot_df <- df %>%
      pivot_longer(cols = -date, names_to = "pollutant", values_to = "concentration")
    
    # Create the plot
    ggplot(plot_df, aes(x = date, y = concentration, color = pollutant)) +
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
  

  output$tempSummary <- renderPrint({
    req(temp_df())
    df <- temp_df()
    
    if(ncol(df) <= 1) {
      return("No temperature data available for the selected parameters.")
    }
    

    summary(df[, -which(names(df) == "date")])
  })
  

  output$airSummary <- renderPrint({
    req(air_df())
    df <- air_df()
    
    if(ncol(df) <= 1) {
      return("No air quality data available for the selected parameters.")
    }
    

    summary(df[, -which(names(df) == "date")])
  })
}



shinyApp(ui = ui, server = server)
