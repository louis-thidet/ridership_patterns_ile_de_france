library(shiny)
# Data Manipulation
library(dplyr)
library(sf)
library(DT)
library(tidyr)
# Plot
library(ggplot2)
# Date management
library(lubridate)
# Cartography
library(leaflet)
library(leaflet.minicharts)
library(mapsf)
# HTML
library(base64enc) # load images

# *****************************************************
# ***** Loading the application's data and assets *****
# *****************************************************
source("scripts/functions.R")
source("scripts/load_data.R")

background_image <- read_and_encode("img/fond.jpg")

redMarker <- makeIcon(
  iconUrl = "img/marker-icon-red.png",
  iconWidth = 27, iconHeight = 83,
)
purpleMarker <- makeIcon(
  iconUrl = "img/marker-icon-purple.png",
  iconWidth = 27, iconHeight = 83,
)
# =========================================================================== UI SIDE OF THE APPLICATION ===========================================================================

# **********************************
# ***** Setting navigation bar *****
# **********************************
ui <- navbarPage(
  "Île-de-France Railway Network Ridership from 2017 to 2023",
  tags$style
  (
    HTML
    ("
      .background_image {
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        z-index: -1;
        background-image: linear-gradient(to bottom, rgba(255, 255, 255, 0) 0%, rgba(255, 255, 255, 1) 110%), url('data:image/png;base64,", background_image, "');
        background-repeat: no-repeat;
        background-size: cover;
      }
    "),
    includeCSS("styles.css"), # Include other css styles
  ),
  div(class = "background_image"),
  
  # **************************
  # ***** Plot Tab Panel *****
  # **************************
  tabPanel
  ("Carte interactive",
    fluidPage
    (
      sidebarLayout
      (
        sidebarPanel
        (
          # Period type
          selectInput(
            inputId = "period_type",
            label = "Type of the Period",
            choices = c("Week" = "week",
                        "Custom period" = "custom"
            ),
          ),
          # Range of close stop area
          sliderInput(
            inputId = "close_range",
            label = "Range of Close Stop Area (m)",
            min = 0, max = 10000,
            value = 2000
          ),
          # Information to show on the plot
          selectInput(
            inputId = "info_type",
            label = "Information to Display",
            choices = c("Chosen Stop Area" = "chosen",
                        "Close Stop Area" = "close"
            ),
            selected = "chosen_stop_area"
          ),
          # WEEK/PERIOD 1 PARAMETERS
          div(
            paste("Period 1 Parameters"), class = "case1"
          ),
          # Stop Area selection
          selectizeInput(
            inputId = "stop1",
            label = "Stop Area",
            choices = "Don't show anything",
            selected = "ABBESSES"
          ),
          # Year selection
          conditionalPanel(
            condition = "input.period_type == 'week'",
            selectInput(
              inputId = "year1",
              label = "Year of the Period",
              choices = NULL,
            ),
          ),
          # Week Type
          conditionalPanel(
            condition = "input.period_type == 'week'",
            selectInput(
              inputId = "week_type1",
              label = "Choose Week Type",
              choices = c("Average Week" = "avg_week",
                          "Average Holiday Week" = "avg_hol_week",
                          "Particular Week" = "particular_week",
                          "Multiple Week" = "multiple_weeks"
              ),
              selected = "avg_week"
            ),
          ),
          # Select the week (when looking for a particular week)
          conditionalPanel(
            condition = "input.week_type1 == 'particular_week' && input.period_type == 'week'",
            sliderInput(
              inputId = "week_number1",
              label = "Select the Week",
              min = 1,
              max = 52,
              value = 20
            ),
          ),
          # Select the weeks (when looking for more than one week)
          conditionalPanel(
            condition = "input.week_type1 == 'multiple_weeks'",
            sliderInput(
              #inputId = "weeks_numbers1",
              "range", "Select the Weeks",
              min = 1, max = 52,
              value = c(1,26)
            ),
          ),
          # WEEK/PERIOD 2 PARAMETERS
          div(
            paste("Period 2 Parameters"), class = "case1bis"
          ),
          # Stop Area selection
          selectizeInput(
            inputId = "stop2",
            label = "Stop Area",
            choices = "Don't show anything",
            selected = "ABBESSES"
          ),
          conditionalPanel(
            condition = "input.period_type == 'custom'",
            div(
              paste("Shared Parameters"), class = "case1ter"
            ),
          ),
          # Year selection
          selectInput(
            inputId = "year2",
            label = "Year of the Period",
            choices = NULL,
          ),
          # Week Type
          conditionalPanel(
            condition = "input.period_type == 'week'",
            selectInput(
              inputId = "week_type2",
              label = "Choose Week Type",
              choices = c("Average Week" = "avg_week",
                          "Average Holiday Week" = "avg_hol_week",
                          "Particular Week" = "particular_week",
                          "Multiple Week" = "multiple_weeks"
              ),
              selected = "avg_week"
            ),
          ),
          # Select the week (when looking for a particular week)
          conditionalPanel(
            condition = "input.week_type2 == 'particular_week' && input.period_type == 'week'",
            sliderInput(
              inputId = "week_number2",
              label = "Select the Week",
              min = 1,
              max = 52,
              value = 20
            ),
          ),
          # Select the weeks (when looking for more than one week)
          conditionalPanel(
            condition = "input.week_type2 == 'multiple_weeks' || input.period_type == 'custom'",
            sliderInput(
              "range_bis", "Select the Weeks",
              min = 1, max = 52,
              value = c(1,26)
            ),
          ),
          tags$input(type = "text", class = "affichage_ventes", value = "non", style = "pointer-events: none;"), # input gérant l'affichage du nombre de ventes
          # KEY NUMBERS
          div(
            htmlOutput("week_vpd")
          ),
        ),
        mainPanel
        (
          # PLOTS
          class = "custom-main-panel",
          leafletOutput("mapPlot",height = "550px", width = "95%"),
          plotOutput("dataPlot", height = "300px", width = "95%"),
        )
      )
    ),
  ),
  
  # **************************
  # ***** Data Tab Panel *****
  # **************************
  tabPanel("Data Tables",
           tabsetPanel(
             id = 'dataset',
             tabPanel("Period 1 Data", DT::dataTableOutput("period1output"), style = "margin-top:20px; padding:15px; background-color:white;"),
             tabPanel("Period 2 Data", DT::dataTableOutput("period2output"), style = "margin-top:20px; padding:15px; background-color:white;"),
             tabPanel("VPD for each Stop Area", DT::dataTableOutput("vpd_aggregated_output"), style = "margin-top:20px; padding:15px; background-color:white;")
           ),
           
           # DOWNLOAD BUTTONS
           conditionalPanel(
             condition = "input.period_type == 'week'",
             downloadButton("week1download", "Week 1 Data", class = "download_button"),
             downloadButton("week2download", "Week 2 Data", class = "download_button"),
             downloadButton("vpd_aggregated_download", "VPD for each Stop Area", class = "download_button")
           ),
           conditionalPanel(
             condition = "input.period_type == 'custom'",
             downloadButton("period1download", "Period 1 Data", class = "download_button"),
             downloadButton("period2download", "Period 2 Data", class = "download_button"),
             downloadButton("vpd_aggregated_download_bis", "VPD for each Stop Area", class = "download_button")
           )
  ),
  
  # **************************
  # **** About Tab Panel *****
  # **************************
  tabPanel("About",
           div(
             paste("About "), class = "about_title"
           ),
           div(
             paste("Application made by Louis THIDET"), class ="about"
           ),
           div(
             HTML(paste("Contact : <a href='mailto: louis.thidet@gmail.com'>louis.thidet@gmail.com</a>")), class ="about"
           ),
  )
)

# =========================================================================== SERVER SIDE OF THE APPLICATION =======================================================================
server <- function(input, output, session) {
  
  # ***************************************************
  # **** Getting the proper choices for the inputs ****
  # ***************************************************
  observe({
    
    # Getting Stop Area choices
    choices <- c("Don't show anything" ,"Average Stop Area", stop_area_list) # Loading the choices takes a  bit of time
    updateSelectizeInput(session, "stop1", choices = choices, server = TRUE)
    updateSelectizeInput(session, "stop2", choices = choices, server = TRUE)
    
    # Getting Year choices
    choices <- c("2017","2018","2019","2020","2021","2022","2023")
    #choices <- unique(substr(vpd_aggregated$JOUR, 1, 4))
    updateSelectInput(session, "year1", choices = choices)
    updateSelectInput(session, "year2", choices = choices)
  })
  
  # *********************************
  # *** DATA FILTERING OF WEEK 1 ****
  # *********************************
  chosen_week1 <- reactive({
    
    # ========================================================
    # The user wants to show the see the information of a week
    # ========================================================
    if (input$period_type == "week" && input$info_type == "chosen") {
      
      # =======================================================
      # The user wants to see the week of the average stop area
      # =======================================================
      if (input$stop1 == "Average Stop Area") {
        
        # Filtering on year
        vpd_aggregated_filtered <- vpd_aggregated %>%
          filter(substr(vpd_aggregated$JOUR, 1, 4) == input$year1)
        
        # Grouping data of each stop area by doing the mean for each day
        vpd_aggregated_filtered <- vpd_aggregated_filtered %>%
          group_by(JOUR) %>%
          summarise(NB_VALD = round(mean(NB_VALD))) %>%
          ungroup() %>%
          as.data.frame()
        
        # ========================================================
        # The user wants to see the week of the selected stop area
        # ========================================================
      } else {
        
        # Filtering to the data of the stop area
        vpd_aggregated_filtered <- vpd_aggregated %>%
          filter(vpd_aggregated$LIBELLE_ARRET == input$stop1)
        
        # Filtering on year
        vpd_aggregated_filtered <- vpd_aggregated_filtered %>%
          filter(substr(vpd_aggregated_filtered$JOUR, 1, 4) == input$year1)
      }
      
      # ================================================================
      # The user wants to see the average week of the selected stop area
      # ================================================================
      if (input$week_type1 == "avg_week") {
        
        week1 <- vpd_aggregated_filtered %>%
          group_by(weekday = weekdays(JOUR)) %>%
          summarise(NB_VALD = round(mean(NB_VALD)))
        
        # ========================================================================
        # The user wants to see the average holiday week of the selected stop area
        # ========================================================================
      } else if (input$week_type1 == "avg_hol_week") {
        
        # Retrieving holidays
        holidays <- get_holidays(input$year1)
        
        # Creating a vector indicating whether each date belongs to holiday periods
        vpd_aggregated_filtered$is_holiday <- vpd_aggregated_filtered$JOUR %in% unlist(holidays)
        
        # Filter the rows corresponding to holidays
        holiday_data <- vpd_aggregated_filtered %>%
          filter(is_holiday)
        
        # Grouping by weekday and calculating the average number of validations
        week1 <- holiday_data %>%
          group_by(weekday = weekdays(as.Date(JOUR))) %>% # weekdays permits to retrieve a date's day
          summarise(NB_VALD = round(mean(NB_VALD)))
        
        # =================================================================
        # The user wants to see a particular week of the selected stop area
        # =================================================================
      } else if (input$week_type1 == "particular_week") {
        
        # Calculating start and end dates for the particular week
        start_date <- as.Date(paste0(input$year1, "-01-01")) + (input$week_number1 - 1) * 7
        end_date <- start_date + days(6)
        
        # Getting the chosen week dates
        particular_week <- seq(start_date, end_date, by = "day")
        
        # Retrieving the week
        week1 <- vpd_aggregated_filtered %>%
          filter(JOUR %in% particular_week) %>%
          group_by(weekday = weekdays(as.Date(JOUR))) %>%
          summarise(NB_VALD = mean(NB_VALD))
        
        # ==================================================================================
        # The user wants to see the average between multiple weeks of the selected stop area
        # ==================================================================================
      } else {
        
        # Calculating start and end dates for the selected week range
        start_date_range <- as.Date(paste0(input$year1, "-01-01")) + (input$range[1] - 1) * 7
        end_date_range <- as.Date(paste0(input$year1, "-01-01")) + (input$range[2] - 1) * 7 + days(6)
        
        # Getting the chosen week dates
        selected_weeks <- seq(start_date_range, end_date_range, by = "day")
        
        # Retrieving the weeks and getting their mean
        week1 <- vpd_aggregated_filtered %>%
          filter(JOUR %in% selected_weeks) %>%
          group_by(weekday = weekdays(as.Date(JOUR))) %>%
          summarise(NB_VALD = round(mean(NB_VALD)))
      }
      
      # ========================
      # Ordering the week's days
      # ========================
      week1$weekday <- factor(week1$weekday, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
      week1 <- week1[order(week1$weekday), ]
      
      # ==========================================================================================
      # The user wants to show the see the information of the stop area closed to the selected one
      # ==========================================================================================
    } else if(input$period_type == "week" && input$info_type == "close") {
      
      # If the selected stop area does have spatial data
      if(nrow(stop_area_locations[stop_area_locations$nom == input$stop1, ]) > 0) {
        
        # Filtering the close stop area
        target_stop_area1 <- stop_area_locations[stop_area_locations$nom == input$stop1, ]
        
        # Create a buffer around the target stop area
        buffer_distance <- input$close_range  # in meters
        buffer_around_target <- st_buffer(target_stop_area1, dist = buffer_distance)
        # Use st_intersects to create a logical vector for filtering
        is_within_buffer <- st_intersects(stop_area_locations, buffer_around_target, sparse = FALSE)
        is_within_buffer_vector <- as.logical(is_within_buffer[, 1])
        # Filter rows based on the logical vector
        stop_area_locations_filtered <- stop_area_locations[is_within_buffer_vector, ]
        
        # Get rid of useless columns
        stop_area_locations_filtered <- stop_area_locations_filtered %>% select(-nom)
        vpd_aggregated_temp <- vpd_aggregated %>% select(-LIBELLE_ARRET)
        
        # Dropping geometry column
        stop_area_locations_filtered <- stop_area_locations_filtered %>%
          st_drop_geometry()
        
        # Adding data and filtering on the chosen year
        stop_area_locations_filtered <- stop_area_locations_filtered %>%
          mutate(idrefa_lda = as.character(idrefa_lda)) %>%
          left_join(vpd_aggregated_temp, by = c("idrefa_lda" = "ID_REFA_LDA")) %>%
          filter(substr(JOUR, 1, 4) == input$year1)
        
        # ==============================================================
        # The user wants to see the average week of the close stop areas
        # ==============================================================
        # Average validation per day
        if(input$week_type1 == "avg_week"){
          
          week1 <- stop_area_locations_filtered %>%
            group_by(weekday = weekdays(JOUR)) %>%
            summarise(NB_VALD = round(mean(NB_VALD)))
          
          # ======================================================================
          # The user wants to see the average holiday week of the close stop areas
          # ======================================================================
        } else if (input$week_type1 == "avg_hol_week") {
          
          # Retrieving holidays
          holidays <- get_holidays(input$year1)
          # Creating a vector indicating whether each date belongs to holiday periods
          stop_area_locations_filtered$is_holiday <- stop_area_locations_filtered$JOUR %in% unlist(holidays)
          # Filter the rows corresponding to holidays
          holiday_data <- stop_area_locations_filtered %>%
            filter(is_holiday)
          # Grouping by weekday and calculating the average number of validations
          week1 <- holiday_data %>%
            group_by(weekday = weekdays(as.Date(JOUR))) %>% # weekdays permits to retrieve a date's day
            summarise(NB_VALD = round(mean(NB_VALD)))
          # Filtering on a particular week
          
          # ===============================================================
          # The user wants to see a particular week of the close stop areas
          # ===============================================================
        } else if (input$week_type1 == "particular_week") {
          
          # Calculating start and end dates for the particular week
          start_date <- as.Date(paste0(input$year1, "-01-01")) + (input$week_number1 - 1) * 7
          end_date <- start_date + days(6)
          
          # Getting the chosen week dates
          particular_week <- seq(start_date, end_date, by = "day")
          
          # Retrieving the week
          week1 <- stop_area_locations_filtered %>%
            filter(JOUR %in% particular_week) %>%
            group_by(weekday = weekdays(as.Date(JOUR))) %>%
            summarise(NB_VALD = mean(NB_VALD))
          
          # ===========================================================================
          # The user wants to see the average of multiple weeks of the close stop areas
          # ===========================================================================
        } else {
          
          # Calculating start and end dates for the selected week range
          start_date_range <- as.Date(paste0(input$year1, "-01-01")) + (input$range[1] - 1) * 7
          end_date_range <- as.Date(paste0(input$year1, "-01-01")) + (input$range[2] - 1) * 7 + days(6)
          
          # Getting the chosen week dates
          selected_weeks <- seq(start_date_range, end_date_range, by = "day")
          
          # Retrieving the weeks and getting their mean
          week1 <- stop_area_locations_filtered %>%
            filter(JOUR %in% selected_weeks) %>%
            group_by(weekday = weekdays(as.Date(JOUR))) %>%
            summarise(NB_VALD = round(mean(NB_VALD)))
        }
        # Making sure the week's days are in the proper order
        week1$weekday <- factor(week1$weekday, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
        week1 <- week1[order(week1$weekday), ]
      } else {
        # Creating an empty data frame, so an error message will appear in the plot
        week1 <- data.frame()
      }
    }
    # Return the Week
    return(week1)
  })
  
  # *********************************
  # ** Data filtering of period 1 ***
  # *********************************
  chosen_custom_period1 <- reactive({
    
    # =======================================================================
    # The user wants to show the information of the second stop area selected
    # =======================================================================
    if(input$period_type == "custom" && input$info_type == "chosen") {
      
      # Calculating start and end dates for the selected weeks range then getting the weeks' dates
      start_date_range <- as.Date(paste0(input$year2, "-01-01")) + (input$range_bis[1] - 1) * 7
      end_date_range <- as.Date(paste0(input$year2, "-01-01")) + (input$range_bis[2] - 1) * 7 + days(6)
      selected_weeks <- seq(start_date_range, end_date_range, by = "day")
      
      # ======================================================================
      # The user wants to see the average validation per day for all stop area
      # ======================================================================
      if(input$stop2 == "Average Stop Area") {
        
        period1 <- vpd_aggregated %>%
          filter(JOUR %in% selected_weeks) %>%
          group_by(JOUR) %>%
          summarize(NB_VALD = round(mean(NB_VALD))) %>%
          ungroup() %>%
          as.data.frame()
        
        # ======================================================================
        # The user wants to see the average validation per day for one stop area
        # ======================================================================
      } else {
        
        # Filtering on selected stop area
        vpd_aggregated_filtered <- vpd_aggregated %>%
          filter(vpd_aggregated$LIBELLE_ARRET == input$stop1)
        # Filtering on year
        period1 <- vpd_aggregated_filtered %>%
          filter(JOUR %in% selected_weeks & substr(JOUR, 1, 4) == input$year2)
      }
    }
    # ========================================================================================
    # The user wants to show the information of the stop areas close to the selected stop area
    # ========================================================================================
    else if(input$period_type == "custom" && input$info_type == "close") {
      
      # If the selected stop area does have spatial data
      if(nrow(stop_area_locations[stop_area_locations$nom == input$stop1, ]) > 0) {
        
        # Filtering the close stop area
        target_stop_area1 <- stop_area_locations[stop_area_locations$nom == input$stop1, ]
        
        # Create a buffer around the target stop area
        buffer_distance <- input$close_range  # in meters
        buffer_around_target <- st_buffer(target_stop_area1, dist = buffer_distance)
        # Use st_intersects to create a logical vector for filtering
        is_within_buffer <- st_intersects(stop_area_locations, buffer_around_target, sparse = FALSE)
        is_within_buffer_vector <- as.logical(is_within_buffer[, 1])
        # Filter rows based on the logical vector
        stop_area_locations_filtered <- stop_area_locations[is_within_buffer_vector, ]
        
        # Calculating start and end dates for the selected weeks range then getting the weeks' dates
        start_date_range <- as.Date(paste0(input$year2, "-01-01")) + (input$range_bis[1] - 1) * 7
        end_date_range <- as.Date(paste0(input$year2, "-01-01")) + (input$range_bis[2] - 1) * 7 + days(6)
        selected_weeks <- seq(start_date_range, end_date_range, by = "day")
        
        # Get rid of useless columns
        stop_area_locations_filtered <- stop_area_locations_filtered %>% select(-nom)
        vpd_aggregated_temp <- vpd_aggregated %>% select(-LIBELLE_ARRET)
        
        # Dropping geometry column
        stop_area_locations_filtered <- stop_area_locations_filtered %>%
          st_drop_geometry()
        
        # Adding data from vpd_aggregated to the close stop areas and filtering on the chosen year
        stop_area_locations_filtered <- stop_area_locations_filtered %>%
          mutate(idrefa_lda = as.character(idrefa_lda)) %>%
          left_join(vpd_aggregated_temp, by = c("idrefa_lda" = "ID_REFA_LDA")) %>%
          filter(substr(JOUR, 1, 4) == input$year2)
        
        # Grouping data per day
        period1 <- stop_area_locations_filtered %>%
          filter(JOUR %in% selected_weeks) %>%
          group_by(JOUR) %>%
          summarize(NB_VALD = round(mean(NB_VALD))) %>%
          ungroup() %>%
          as.data.frame()
        
        # If the selected stop area doesn't have spatial data
      } else {
        # Creating an empty data frame, so an error message will appear in the plot
        period1 <- data.frame()
      }
    }
    # Return the Period
    return(period1)
  })
  
  # *********************************
  # *** Data filtering of week 2 ****
  # *********************************
  chosen_week2 <- reactive({
    
    # ========================================================
    # The user wants to show the see the information of a week
    # ========================================================
    if (input$period_type == "week" && input$info_type == "chosen") {
      
      # =======================================================
      # The user wants to see the week of the average stop area
      # =======================================================
      if (input$stop2 == "Average Stop Area") {
        
        # Filtering on year
        vpd_aggregated_filtered <- vpd_aggregated %>%
          filter(substr(vpd_aggregated$JOUR, 1, 4) == input$year2)
        
        # Grouping data of each stop area by doing the mean for each day
        vpd_aggregated_filtered <- vpd_aggregated_filtered %>%
          group_by(JOUR) %>%
          summarise(NB_VALD = round(mean(NB_VALD))) %>%
          ungroup() %>%
          as.data.frame()
        
        # ========================================================
        # The user wants to see the week of the selected stop area
        # ========================================================
      } else {
        
        # Filtering to the data of the stop area
        vpd_aggregated_filtered <- vpd_aggregated %>%
          filter(vpd_aggregated$LIBELLE_ARRET == input$stop2)
        
        # Filtering on year
        vpd_aggregated_filtered <- vpd_aggregated_filtered %>%
          filter(substr(vpd_aggregated_filtered$JOUR, 1, 4) == input$year2)
      }
      
      # ================================================================
      # The user wants to see the average week of the selected stop area
      # ================================================================
      if (input$week_type2 == "avg_week") {
        
        week2 <- vpd_aggregated_filtered %>%
          group_by(weekday = weekdays(JOUR)) %>%
          summarise(NB_VALD = round(mean(NB_VALD)))
        
        # ========================================================================
        # The user wants to see the average holiday week of the selected stop area
        # ========================================================================
      } else if (input$week_type2 == "avg_hol_week") {
        
        # Retrieving holidays
        holidays <- get_holidays(input$year2)
        
        # Creating a vector indicating whether each date belongs to holiday periods
        vpd_aggregated_filtered$is_holiday <- vpd_aggregated_filtered$JOUR %in% unlist(holidays)
        
        # Filter the rows corresponding to holidays
        holiday_data <- vpd_aggregated_filtered %>%
          filter(is_holiday)
        
        # Grouping by weekday and calculating the average number of validations
        week2 <- holiday_data %>%
          group_by(weekday = weekdays(as.Date(JOUR))) %>% # weekdays permits to retrieve a date's day
          summarise(NB_VALD = round(mean(NB_VALD)))
        
        # =================================================================
        # The user wants to see a particular week of the selected stop area
        # =================================================================
      } else if (input$week_type2 == "particular_week") {
        
        # Calculating start and end dates for the particular week
        start_date <- as.Date(paste0(input$year2, "-01-01")) + (input$week_number2 - 1) * 7  ###
        end_date <- start_date + days(6)
        
        # Getting the chosen week dates
        particular_week <- seq(start_date, end_date, by = "day")
        
        # Retrieving the week
        week2 <- vpd_aggregated_filtered %>%
          filter(JOUR %in% particular_week) %>%
          group_by(weekday = weekdays(as.Date(JOUR))) %>%
          summarise(NB_VALD = mean(NB_VALD))
        
        # ==================================================================================
        # The user wants to see the average between multiple weeks of the selected stop area
        # ==================================================================================
      } else {
        
        # Calculating start and end dates for the selected week range
        start_date_range <- as.Date(paste0(input$year2, "-01-01")) + (input$range_bis[1] - 1) * 7
        end_date_range <- as.Date(paste0(input$year2, "-01-01")) + (input$range_bis[2] - 1) * 7 + days(6)
        
        # Getting the chosen week dates
        selected_weeks <- seq(start_date_range, end_date_range, by = "day")
        
        # Retrieving the weeks and getting their mean
        week2 <- vpd_aggregated_filtered %>%
          filter(JOUR %in% selected_weeks) %>%
          group_by(weekday = weekdays(as.Date(JOUR))) %>%
          summarise(NB_VALD = round(mean(NB_VALD)))
      }
      
      # ========================
      # Ordering the week's days
      # ========================
      week2$weekday <- factor(week2$weekday, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
      week2 <- week2[order(week2$weekday), ]
      
      # ==========================================================================================
      # The user wants to show the see the information of the stop area closed to the selected one
      # ==========================================================================================
    } else if(input$period_type == "week" && input$info_type == "close") {
      
      # If the selected stop area doesn't have spatial data
      if(nrow(stop_area_locations[stop_area_locations$nom == input$stop2, ]) > 0) {
        
        # Filtering the close stop area
        target_stop_area2 <- stop_area_locations[stop_area_locations$nom == input$stop2, ]
        
        # Create a buffer around the target stop area
        buffer_distance <- input$close_range  # in meters
        buffer_around_target <- st_buffer(target_stop_area2, dist = buffer_distance)
        # Use st_intersects to create a logical vector for filtering
        is_within_buffer <- st_intersects(stop_area_locations, buffer_around_target, sparse = FALSE)
        is_within_buffer_vector <- as.logical(is_within_buffer[, 1])
        # Filter rows based on the logical vector
        stop_area_locations_filtered <- stop_area_locations[is_within_buffer_vector, ]
        
        # Get rid of useless columns
        stop_area_locations_filtered <- stop_area_locations_filtered %>% select(-nom)
        vpd_aggregated_temp <- vpd_aggregated %>% select(-LIBELLE_ARRET)
        
        # Dropping geometry column
        stop_area_locations_filtered <- stop_area_locations_filtered %>%
          st_drop_geometry()
        
        # Adding data and filtering on the chosen year
        stop_area_locations_filtered <- stop_area_locations_filtered %>%
          mutate(idrefa_lda = as.character(idrefa_lda)) %>%
          left_join(vpd_aggregated_temp, by = c("idrefa_lda" = "ID_REFA_LDA")) %>%
          filter(substr(JOUR, 1, 4) == input$year2)
        
        # ==============================================================
        # The user wants to see the average week of the close stop areas
        # ==============================================================
        if(input$week_type2 == "avg_week"){
          
          week2 <- stop_area_locations_filtered %>%
            group_by(weekday = weekdays(JOUR)) %>%
            summarise(NB_VALD = round(mean(NB_VALD)))
          
          # ======================================================================
          # The user wants to see the average holiday week of the close stop areas
          # ======================================================================
        } else if (input$week_type2 == "avg_hol_week") {
          
          # Retrieving holidays
          holidays <- get_holidays(input$year2)
          # Creating a vector indicating whether each date belongs to holiday periods
          stop_area_locations_filtered$is_holiday <- stop_area_locations_filtered$JOUR %in% unlist(holidays)
          # Filter the rows corresponding to holidays
          holiday_data <- stop_area_locations_filtered %>%
            filter(is_holiday)
          # Grouping by weekday and calculating the average number of validations
          week2 <- holiday_data %>%
            group_by(weekday = weekdays(as.Date(JOUR))) %>% # weekdays permits to retrieve a date's day
            summarise(NB_VALD = round(mean(NB_VALD)))
          
          # ===============================================================
          # The user wants to see a particular week of the close stop areas
          # ===============================================================
        } else if (input$week_type2 == "particular_week") {
          
          # Calculating start and end dates for the particular week
          start_date <- as.Date(paste0(input$year2, "-01-01")) + (input$week_number2 - 1) * 7
          end_date <- start_date + days(6)
          
          # Getting the chosen week dates
          particular_week <- seq(start_date, end_date, by = "day")
          
          # Retrieving the week
          week2 <- stop_area_locations_filtered %>%
            filter(JOUR %in% particular_week) %>%
            group_by(weekday = weekdays(as.Date(JOUR))) %>%
            summarise(NB_VALD = mean(NB_VALD))
          
          # ===========================================================================
          # The user wants to see the average of multiple weeks of the close stop areas
          # ===========================================================================
        } else {
          # Calculating start and end dates for the selected week range
          start_date_range <- as.Date(paste0(input$year2, "-01-01")) + (input$range_bis[1] - 1) * 7
          end_date_range <- as.Date(paste0(input$year2, "-01-01")) + (input$range_bis[2] - 1) * 7 + days(6)
          
          # Getting the chosen week dates
          selected_weeks <- seq(start_date_range, end_date_range, by = "day")
          
          # Retrieving the weeks and getting their mean
          week2 <- stop_area_locations_filtered %>%
            filter(JOUR %in% selected_weeks) %>%
            group_by(weekday = weekdays(as.Date(JOUR))) %>%
            summarise(NB_VALD = round(mean(NB_VALD)))
        }
        # Making sure the week's days are in the proper order
        week2$weekday <- factor(week2$weekday, levels= c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
        week2 <- week2[order(week2$weekday), ]
      } else {
        # Creating an empty data frame, so an error message will appear in the plot
        week2 <- data.frame()
      }
    }
    # Return the Week
    return(week2)
  })
  
  
  # *********************************
  # ** Data filtering of period 2 ***
  # *********************************
  chosen_custom_period2 <- reactive({
    
    # =======================================================================
    # The user wants to show the information of the second stop area selected
    # =======================================================================
    if(input$period_type == "custom" && input$info_type == "chosen") {
      
      # Calculating start and end dates for the selected weeks range then getting the weeks' dates
      start_date_range <- as.Date(paste0(input$year2, "-01-01")) + (input$range_bis[1] - 1) * 7
      end_date_range <- as.Date(paste0(input$year2, "-01-01")) + (input$range_bis[2] - 1) * 7 + days(6)
      selected_weeks <- seq(start_date_range, end_date_range, by = "day")
      
      # ======================================================================
      # The user wants to see the average validation per day for all stop area
      # ======================================================================
      if(input$stop2 == "Average Stop Area") {
        
        period2 <- vpd_aggregated %>%
          filter(JOUR %in% selected_weeks) %>%
          group_by(JOUR) %>%
          summarize(NB_VALD = round(mean(NB_VALD))) %>%
          ungroup() %>%
          as.data.frame()
        
        # ======================================================================
        # The user wants to see the average validation per day for one stop area
        # ======================================================================
      } else {
        
        # Filtering on selected stop area
        vpd_aggregated_filtered <- vpd_aggregated %>%
          filter(vpd_aggregated$LIBELLE_ARRET == input$stop2)
        # Filtering on year
        period2 <- vpd_aggregated_filtered %>%
          filter(JOUR %in% selected_weeks & substr(JOUR, 1, 4) == input$year2)
      }
    }
    # ========================================================================================
    # The user wants to show the information of the stop areas close to the selected stop area
    # ========================================================================================
    else if(input$period_type == "custom" && input$info_type == "close") {
      
      # If the selected stop area does have spatial data
      if(nrow(stop_area_locations[stop_area_locations$nom == input$stop2, ]) > 0) {
        
        # Filtering the close stop area
        target_stop_area2 <- stop_area_locations[stop_area_locations$nom == input$stop2, ]
        
        # Create a buffer around the target stop area
        buffer_distance <- input$close_range  # in meters
        buffer_around_target <- st_buffer(target_stop_area2, dist = buffer_distance)
        # Use st_intersects to create a logical vector for filtering
        is_within_buffer <- st_intersects(stop_area_locations, buffer_around_target, sparse = FALSE)
        is_within_buffer_vector <- as.logical(is_within_buffer[, 1])
        # Filter rows based on the logical vector
        stop_area_locations_filtered <- stop_area_locations[is_within_buffer_vector, ]
        
        # Calculating start and end dates for the selected weeks range then getting the weeks' dates
        start_date_range <- as.Date(paste0(input$year2, "-01-01")) + (input$range_bis[1] - 1) * 7
        end_date_range <- as.Date(paste0(input$year2, "-01-01")) + (input$range_bis[2] - 1) * 7 + days(6)
        selected_weeks <- seq(start_date_range, end_date_range, by = "day")
        
        # Get rid of useless columns
        stop_area_locations_filtered <- stop_area_locations_filtered %>% select(-nom)
        vpd_aggregated_temp <- vpd_aggregated %>% select(-LIBELLE_ARRET)
        
        # Dropping geometry column
        stop_area_locations_filtered <- stop_area_locations_filtered %>%
          st_drop_geometry()
        
        # Adding data from vpd_aggregated to the close stop areas and filtering on the chosen year
        stop_area_locations_filtered <- stop_area_locations_filtered %>%
          mutate(idrefa_lda = as.character(idrefa_lda)) %>%
          left_join(vpd_aggregated_temp, by = c("idrefa_lda" = "ID_REFA_LDA")) %>%
          filter(substr(JOUR, 1, 4) == input$year2)
        
        # Grouping data per day
        period2 <- stop_area_locations_filtered %>%
          filter(JOUR %in% selected_weeks) %>%
          group_by(JOUR) %>%
          summarize(NB_VALD = round(mean(NB_VALD))) %>%
          ungroup() %>%
          as.data.frame()
        
        # If the selected stop area doesn't have spatial data
      } else {
        # Creating an empty data frame, so an error message will appear in the plot
        period2 <- data.frame()
      }
    }
    # Return the Period
    return(period2)
  })
  
  # *******************************************
  # ************* Plot creation ***************
  # *******************************************
  output$dataPlot <- renderPlot({
    
    # =============================
    # The user wants to show a week
    # =============================
    if(input$period_type == "week"){
      
      # Create a factor with the proper order of weekdays
      weekday_order <- factor(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered = TRUE)
      
      data1 <- chosen_week1()
      data2 <- chosen_week2()
      
      # =============================================
      # Error message : no data selected or available
      # =============================================
      if (nrow(data1) == 0 && nrow(data2) == 0) {
        
        if (input$stop1 == "Don't show anything" && input$stop2 == "Don't show anything") {
          label <- "No data selected"
        } else {
          label <- "No data available for selected settings"
        }
        ggplot() +
          geom_text(aes(x = 0.5, y = 0.5, label = label),
                    size = 6, color = "red", hjust = 0.5, vjust = 0.5) +
          theme_void()  # Remove axis and labels
        
        # ==========================
        # Creating a plot for week 1
        # ==========================
      } else if (nrow(data1) > 0 && nrow(data2) == 0) {
        
        ggplot(data1, aes(x = factor(weekday, levels = weekday_order), y = NB_VALD, fill = "Week 1")) +
          geom_bar(stat = "identity", position = "dodge", alpha = 0.5) +
          labs(title = "Comparison of two selected weeks",
               x = "Day of the week",
               y = "Average validations",
               fill = "Legend") +
          scale_y_continuous(labels = scales::comma_format()) +
          scale_fill_manual(values = c("Week 1" = "red")) +
          theme_minimal() +
          theme(plot.title = element_text(size = 15), text = element_text(size = 13))
        
        # ==========================
        # Creating a plot for week 2
        # ==========================
      } else if (nrow(data1) == 0 && nrow(data2) > 0) {
        
        ggplot(data2, aes(x = factor(weekday, levels = weekday_order), y = NB_VALD, fill = "Week 2")) +
          geom_bar(stat = "identity", position = "dodge", alpha = 0.5) +
          labs(title = "Comparison of two selected weeks",
               x = "Day of the week",
               y = "Average validations",
               fill = "Legend") +
          scale_y_continuous(labels = scales::comma_format()) +
          scale_fill_manual(values = c("Week 2" = "blue")) +
          theme_minimal() +
          theme(plot.title = element_text(size = 15), text = element_text(size = 13))
        
        # =============================
        # Creating a plot for both week
        # =============================
      } else {
        
        combined_data <- rbind(transform(data1, Week = "Week 1"), transform(data2, Week = "Week 2"))
        
        ggplot(combined_data, aes(x = factor(weekday, levels = unique(weekday_order)), y = NB_VALD, fill = Week)) +
          geom_bar(stat = "identity", position = "dodge", alpha = 0.5) +
          labs(title = "Comparison of two selected weeks",
               x = "Day of the week",
               y = "Average validations",
               fill = "Legend") +
          scale_y_continuous(labels = scales::comma_format()) +
          scale_fill_manual(values = c("Week 1" = "red", "Week 2" = "blue")) +
          theme_minimal() +
          theme(plot.title = element_text(size = 15), text = element_text(size = 13))
      }
      
      # ======================================
      # The user wants to show a custom period
      # ======================================
    } else {
      data1 <- chosen_custom_period1()
      data2 <- chosen_custom_period2()
      
      data1$NB_VALD <- as.numeric(data1$NB_VALD)
      data2$NB_VALD <- as.numeric(data2$NB_VALD)
      avg_vpd1 <- round(mean(data1$NB_VALD))
      avg_vpd2 <- round(mean(data2$NB_VALD))
      
      holiday_data <- convert_holidays(get_holidays(input$year2))
      
      # =============================================
      # Error message : no data selected or available
      # =============================================
      if (nrow(data1) == 0 && nrow(data2) == 0) {
        
        if (input$stop1 == "Don't show anything" && input$stop2 == "Don't show anything") {
          label <- "No data selected"
        } else {
          label <- "No data available for selected stop areas and years"
        }
        ggplot() +
          geom_text(aes(x = 0.5, y = 0.5, label = label),
                    size = 6, color = "red", hjust = 0.5, vjust = 0.5) +
          theme_void()  # Remove axis and labels
        
        # ============================
        # Creating a plot for period 1
        # ============================
      } else if (nrow(data1) > 0 && nrow(data2) == 0) {
        
        if(input$stop1 != "Average Stop Area"){
          stop_area_name <- paste0(" at ", input$stop1)
        } else {
          stop_area_name <- " (Average Stop Area)"
        }
        
        ggplot(data1, aes(x = JOUR, y = NB_VALD)) +
          # Line charts for the the period
          geom_line(stat = "summary", fun = mean, color = 'grey', linewidth = 1) +
          
          # Vertical lines to display holiday periods
          geom_vline(data = holiday_data, aes(xintercept = start_date, color = label), linewidth = 0.7) +
          geom_vline(data = holiday_data, aes(xintercept = end_date, color = label), linewidth = 0.7) +
          
          # Horizontal line to display average validation per day
          geom_hline(yintercept = avg_vpd1, linetype = "dashed", color = "#fe7f7e", linewidth = 1) +
          
          # Theme
          theme_minimal() +
          scale_y_continuous(labels = function(x) format(x, scientific = FALSE, trim = TRUE)) +
          
          # Titles
          labs(
            title = paste0("Validations per Day in ", input$year2, " (weeks ", input$range_bis[1], "-", input$range_bis[2],")" , stop_area_name),
            x = "Months",
            y = "Number of Validations"
          ) +
          guides(color = guide_legend(title = "Holidays")) +
          theme(plot.title = element_text(size = 15), text = element_text(size = 13))
        
        # ============================
        # Creating a plot for period 2
        # ============================
      } else if (nrow(data1) == 0 && nrow(data2) > 0) {
        
        if(input$stop1 != "Average Stop Area"){
          stop_area_name <- paste0(" at ", input$stop2)
        } else {
          stop_area_name <- " (Average Stop Area)"
        }
        
        ggplot(data2, aes(x = JOUR, y = NB_VALD)) +
          # Line charts for the the period
          geom_line(stat = "summary", fun = mean, color = 'grey', linewidth = 1) +
          
          # Vertical lines to display holiday periods
          geom_vline(data = holiday_data, aes(xintercept = start_date, color = label), linewidth = 0.7) +
          geom_vline(data = holiday_data, aes(xintercept = end_date, color = label), linewidth = 0.7) +
          
          # Horizontal lines to display average validation per day
          geom_hline(yintercept = avg_vpd2, linetype = "dashed", color = "#7e7eff", linewidth = 1) +
          
          # Theme
          theme_minimal() +
          scale_y_continuous(labels = function(x) format(x, scientific = FALSE, trim = TRUE)) +
          
          # Titles
          labs(
            title = paste0("Validations per Day in ", input$year2, " (weeks ", input$range_bis[1], "-", input$range_bis[2],")" , stop_area_name),
            x = "Months",
            y = "Number of Validations"
          ) +
          guides(color = guide_legend(title = "Holidays")) +
          theme(plot.title = element_text(size = 15), text = element_text(size = 13))
        
        # ===============================
        # Creating a plot for both period
        # ===============================
      } else {
        
        if (input$stop1 != "Average Stop Area" && input$stop2 != "Average Stop Area") {
          if (input$info_type == "chosen") {
            stop_area_name <- paste0(" at ", input$stop1, " and ", input$stop2)
          } else {
            stop_area_name <- paste0(" around ", input$stop1, " and ", input$stop2)
          }
        } else {
          stop_area_name <- " (Average Stop Area)"
        }
        stop_area_name1 <- input$stop1
        stop_area_name2 <- input$stop2
        
        ggplot() +
          # Line charts for the two periods
          geom_line(data = data1, aes(x = JOUR, y = NB_VALD, color = stop_area_name1), stat = "summary", fun = mean, size = 1) +
          geom_line(data = data2, aes(x = JOUR, y = NB_VALD, color = stop_area_name2), stat = "summary", fun = mean, size = 1) +
          
          # Vertical lines to display holiday periods
          geom_vline(data = holiday_data, aes(xintercept = start_date), linewidth = 0.7) +
          geom_vline(data = holiday_data, aes(xintercept = end_date), linewidth = 0.7) +
          
          # Horizontal lines to display average validation per day
          geom_hline(yintercept = avg_vpd1, linetype = "dashed", color = "#fe7f7e", linewidth = 1) +
          geom_hline(yintercept = avg_vpd2, linetype = "dashed", color = "#7e7eff", linewidth = 1) +
          
          # Theme
          theme_minimal() +
          scale_y_continuous(labels = function(x) format(x, scientific = FALSE, trim = TRUE)) +
          
          # Titles
          labs(
            title = paste0("Validations per Day in ", input$year2, " (weeks ", input$range_bis[1], "-", input$range_bis[2],")" , stop_area_name),
            x = "Months",
            y = "Number of Validations"
          ) +
          guides(color = guide_legend(title = "Stop Area")) +
          theme(plot.title = element_text(size = 15), text = element_text(size = 13)) +
          
          # Specify colors for data1 and data2
          scale_color_manual(values = c("#fe7f7e", "#7e7eff"))
      }
    }
  })
  
  # ******************************************
  # ************* Map creation ***************
  # ******************************************
  output$mapPlot <- renderLeaflet({
    
    # ================
    # Creating the map
    # ================
    carte <- leaflet() %>%
      # Tiles
      addTiles() %>%
      addTiles(group = "ESRI topo.") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "ESRI topo.") %>%
      addProviderTiles("Esri.WorldImagery", group = "ESRI photo.") %>%
      # Tiles control
      addLayersControl(
        baseGroups = c("ESRI topo.", "ESRI photo.", "OSM"),
        position = "bottomright"
      )
    
    # ==================================================================
    # Checking if at least one of the chosen stop area have spatial data
    # ==================================================================
    if ((any(stop_area_locations$nom == input$stop1) || any(stop_area_locations$nom == input$stop2))) {
      
      # Filtering on the stop area(s)
      stop_area_locations_filtered <- stop_area_locations %>%
        filter(nom == input$stop1 | nom == input$stop2)
      
      # ========================================
      # The user wants to display two stop areas
      # ========================================
      if ((any(stop_area_locations_filtered$nom == input$stop1) && any(stop_area_locations_filtered$nom == input$stop2))) {
        
        # Getting the stop areas close to the selected
        
        # Filter rows based on the target stop area ID
        target_stop_area1 <- stop_area_locations[stop_area_locations$nom == input$stop1, ]
        # Create a buffer around the target stop area
        buffer_distance <- input$close_range  # in meters
        buffer_around_target <- st_buffer(target_stop_area1, dist = buffer_distance)
        # Use st_intersects to create a logical vector for filtering
        is_within_buffer <- st_intersects(stop_area_locations, buffer_around_target, sparse = FALSE)
        is_within_buffer_vector <- as.logical(is_within_buffer[, 1])
        # Filter rows based on the logical vector
        stop_area_locations_filtered_bis <- stop_area_locations[is_within_buffer_vector, ]
        
        # Repeating the operation
        target_stop_area2 <- stop_area_locations[stop_area_locations$nom == input$stop2, ]
        buffer_distance <- input$close_range
        buffer_around_target <- st_buffer(target_stop_area2, dist = buffer_distance)
        is_within_buffer <- st_intersects(stop_area_locations, buffer_around_target, sparse = FALSE)
        is_within_buffer_vector <- as.logical(is_within_buffer[, 1])
        stop_area_locations_filtered_ter <- stop_area_locations[is_within_buffer_vector, ]
        
        # Set view parameters
        latitude <- mean(stop_area_locations_filtered$latitude)
        longitude <- mean(stop_area_locations_filtered$longitude)
        zoom = 10
        
        # Creating close stop areas dataframe
        close_stop_areas <- bind_rows(stop_area_locations_filtered_bis, stop_area_locations_filtered_ter)
        close_stop_areas <- close_stop_areas %>% filter(nom != input$stop1 & nom != input$stop2)
        
        # =======================================
        # The user wants to display one stop area
        # =======================================
      } else {
        
        # Checking which input holds a stop area
        if (input$stop1 %in% stop_area_locations$nom) {
          target_stop_area <- stop_area_locations[stop_area_locations$nom == input$stop1, ]
        } else if (input$stop2 %in% stop_area_locations$nom) {
          target_stop_area <- stop_area_locations[stop_area_locations$nom == input$stop2, ]
        } 
        
        # Getting the stop area close to the selected stop area
        buffer_distance <- input$close_range
        buffer_around_target <- st_buffer(target_stop_area, dist = buffer_distance)
        is_within_buffer <- st_intersects(stop_area_locations, buffer_around_target, sparse = FALSE)
        is_within_buffer_vector <- as.logical(is_within_buffer[, 1])
        close_stop_areas <- stop_area_locations[is_within_buffer_vector, ]
        
        # Set view parameters
        latitude <- stop_area_locations_filtered$latitude[1]
        longitude <- stop_area_locations_filtered$longitude[1]
        zoom = 13
        
        # Creating close stop areas dataframe
        close_stop_areas <- close_stop_areas %>% filter(nom != input$stop1 & nom != input$stop2)
      }
      
      # Dropping geometry, as latitude and longitude will be used to represent. It's not needed
      # as the stop area, which will be displayed as markers.
      close_stop_areas <- close_stop_areas %>%
        st_drop_geometry()
      stop_area_locations_filtered <- stop_area_locations_filtered %>%
        st_drop_geometry()
      
      # =========================
      # No close area stop founds
      # =========================
      # If there is not enough range, they can be no row around the selected stop areas, thus no stop areas in the dataframe
      if(nrow(close_stop_areas) == 0){
        carte <- carte %>%
          setView(lat = latitude, lng = longitude, zoom = zoom) 
        
        # If there are two selected stop areas
        if ((any(stop_area_locations_filtered$nom == input$stop1) && any(stop_area_locations_filtered$nom == input$stop2))) {
          
          # Removing geometry
          target_stop_area1 <- target_stop_area1 %>%
            st_drop_geometry()
          target_stop_area2 <- target_stop_area2 %>%
            st_drop_geometry()
          
          # Adding markers for stop areas
          carte <- carte %>%
            addMarkers(data = target_stop_area1, icon = redMarker,
                       label = ~nom) %>%
            addMarkers(data = target_stop_area2, icon = purpleMarker,
                       label = ~nom)
          
          # If there are is one stop area selected (input$stop1)
        } else if (any(stop_area_locations_filtered$nom == input$stop1)) {
          
          carte <- carte %>%
            addMarkers(data = stop_area_locations_filtered, icon = redMarker,
                       label = ~nom)
          
          # If there are is one stop area selected (input$stop2)
        } else {
          
          carte <- carte %>%
            addMarkers(data = stop_area_locations_filtered, icon = purpleMarker,
                       label = ~nom)
        }
        # ==========================
        # Close stop area are founds
        # ==========================
      } else {
        
        # Adding markers for near stop area
        carte <- carte %>%
          addMarkers(data = close_stop_areas,
                     label = ~nom)
        
        # Case where two stop area are chosen
        if ((any(stop_area_locations_filtered$nom == input$stop1) && any(stop_area_locations_filtered$nom == input$stop2))) {
          
          # Removing geometry
          target_stop_area1 <- target_stop_area1 %>%
            st_drop_geometry()
          target_stop_area2 <- target_stop_area2 %>%
            st_drop_geometry()
          
          carte <- carte %>%
            addMarkers(data = target_stop_area1, icon = redMarker,
                       label = ~nom) %>%
            addMarkers(data = target_stop_area2, icon = purpleMarker,
                       label = ~nom)
          # One stop area
        } else if (any(stop_area_locations_filtered$nom == input$stop1)) {
          
          carte <- carte %>%
            addMarkers(data = stop_area_locations_filtered, icon = redMarker,
                       label = ~nom)
          # One stop area
        } else {
          
          carte <- carte %>%
            addMarkers(data = stop_area_locations_filtered, icon = purpleMarker,
                       label = ~nom)
        }
      }
    } else {
      # Default view, when no stop area are selected
      carte <- carte %>%
        setView(lat = 48.853823, lng = 2.344152, zoom = 10)
    }
  })
  
  # ***********************************************
  # ************Average number display ************
  # ***********************************************
  output$week_vpd <- renderText({
    
    if(input$period_type == "week"){
      data1 <- chosen_week1()
      data2 <- chosen_week2()
      period <- "Week"
    } else {
      data1 <- chosen_custom_period1()
      data2 <- chosen_custom_period2()
      period <- "Period"
    }
    
    if (nrow(data1) != 0 || nrow(data2) != 0) {
      content <- if (nrow(data1) == 0) {
        paste("<div class='case2'>Mean Validation per day: <br>", period, " 2: ", round(mean(data2$NB_VALD)), "<br></div>")
      } else if (nrow(data2) == 0) {
        paste("<div class='case2'>Mean Validation per day: <br>", period, " 1: ", round(mean(data1$NB_VALD)), "<br></div>")
      } else {
        paste("<div class='case2'>Mean Validation per day: <br>",
              "Week 1: ", round(mean(data1$NB_VALD)), "<br>",
              "Week 2: ", round(mean(data2$NB_VALD)), "<br></div>")
      }
    }
  })
  
  # ***********************************************
  # ************* Download handlers ***************
  # ***********************************************
  output$week1download <- downloadHandler(
    filename = function() {
      paste("week1-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(chosen_week1(), file)
    }
  )
  output$period1download <- downloadHandler(
    filename = function() {
      paste("custom_period1-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(chosen_custom_period1(), file)
    }
  )
  output$week2download <- downloadHandler(
    filename = function() {
      paste("week2-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(chosen_week2(), file)
    }
  )
  output$period2download <- downloadHandler(
    filename = function() {
      paste("custom_period2-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(chosen_custom_period2(), file)
    }
  )
  output$vpd_aggregated_download <- downloadHandler(
    filename = function() {
      paste("vpd_all_stop_area-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(vpd_aggregated, file)
    }
  )
  output$vpd_aggregated_download_bis <- downloadHandler(
    filename = function() {
      paste("vpd_all_stop_area-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(vpd_aggregated, file)
    }
  )
  # *************************************************
  # ************* Data Tables outputs ***************
  # *************************************************
  output$period1output = DT::renderDataTable({
    if (input$period_type == "week") {
      chosen_week1()
    } else {
      chosen_custom_period1()
    }
  })
  output$period2output = DT::renderDataTable({
    if (input$period_type == "week") {
      chosen_week2()
    } else {
      chosen_custom_period2()
    }
  })
  output$vpd_aggregated_output = DT::renderDataTable({
    vpd_aggregated
  })
}

## =================================================== LANCEMENT DE L'APPLICATION ===================================================
shinyApp(ui = ui, server = server)