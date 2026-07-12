# app.R
# Mobile Device Usage Explorer
# ST 558 Project - Shiny App

library(shiny)
library(tidyverse)
library(bslib)
library(DT)
library(shinythemes)
library(shinycssloaders)  

# Load and prepare data
device_usage_data <- read_csv("user_behavior_dataset.csv")

# Create grouped variables
device_usage_data <- device_usage_data |>
  mutate(
    # Age groups 
    Age_Group = case_when(
      Age < 25   ~ "Young (18-24)",
      Age < 35   ~ "Young Adult (25-34)",
      Age < 45   ~ "Middle Adult (35-44)",
      Age < 55   ~ "Older Adult (45-54)",
      Age >= 55  ~ "Senior (55-59)"
    ),
    
    # Screen On Time groups 
    Screen_Time_Group = case_when(
      `Screen On Time (hours/day)` < 2        ~ "Very Light (0-2h)",
      `Screen On Time (hours/day)` < 4        ~ "Light (2-4h)",
      `Screen On Time (hours/day)` < 6        ~ "Moderate (4-6h)",
      `Screen On Time (hours/day)` < 8        ~ "Heavy (6-8h)",
      `Screen On Time (hours/day)` >= 8       ~ "Very Heavy/Extreme (8-12h)"
    ),
    
    # App Usage Time 
    App_Usage_Group = case_when(
      `App Usage Time (min/day)` < 100        ~ "Very Light (0-100m)",
      `App Usage Time (min/day)` < 200        ~ "Light (100-200m)",
      `App Usage Time (min/day)` < 300        ~ "Moderate (200-300m)",
      `App Usage Time (min/day)` < 400        ~ "Heavy (300-400m)",
      `App Usage Time (min/day)` <= 600       ~ "Very Heavy (400-600m)"
    ),
    
    # Battery Drain 
    Battery_Group = case_when(
      `Battery Drain (mAh/day)` < 500         ~ "Very Low (0-500mAh)",
      `Battery Drain (mAh/day)` < 1000        ~ "Low (500-1000mAh)",
      `Battery Drain (mAh/day)` < 1500        ~ "Moderate (1000-1500mAh)",
      `Battery Drain (mAh/day)` < 2000        ~ "Heavy (1500-2000mAh)",
      `Battery Drain (mAh/day)` <= 3000       ~ "Very Heavy (2000-3000mAh)"
    ),
    
    # Convert to factors
    Age_Group = factor(Age_Group, 
                       levels = c("Young (18-24)", "Young Adult (25-34)", 
                                  "Middle Adult (35-44)", "Older Adult (45-54)", 
                                  "Senior (55-59)")),
    Screen_Time_Group = factor(Screen_Time_Group, 
                               levels = c("Very Light (0-2h)", "Light (2-4h)", 
                                          "Moderate (4-6h)", "Heavy (6-8h)", 
                                          "Very Heavy/Extreme (8-12h)")),
    App_Usage_Group = factor(App_Usage_Group,
                             levels = c("Very Light (0-100m)", "Light (100-200m)",
                                        "Moderate (200-300m)", "Heavy (300-400m)",
                                        "Very Heavy (400-600m)")),
    Battery_Group = factor(Battery_Group,
                           levels = c("Very Low (0-500mAh)", "Low (500-1000mAh)",
                                      "Moderate (1000-1500mAh)", "Heavy (1500-2000mAh)",
                                      "Very Heavy (2000-3000mAh)"))
  )


# define the ui 
ui <- page_sidebar(
  title = "Mobile Device Usage Explorer",
  windowTitle = "Device Usage App",
  theme = bs_theme(preset = "minty"),
  
  # sidebar with input controls
  sidebar = sidebar(
    width = 350,
    
    h4("Data Subset Controls", class = "text-primary"),
    p("Select the criteria to filter the data, then click 'Apply Filters'", 
      style = "font-size: 0.9em; color: #666;"),
    hr(),
    
    # categorical variable 1: Device Model
    h5("Device Model:"),
    checkboxGroupInput(
      inputId = "device_filter",
      label = NULL,
      choices = c("All" = "all", 
                  "Google Pixel 5" = "Google Pixel 5",
                  "OnePlus 9" = "OnePlus 9",
                  "Xiaomi Mi 11" = "Xiaomi Mi 11",
                  "Samsung Galaxy S21" = "Samsung Galaxy S21",
                  "iPhone 12" = "iPhone 12"),
      selected = "all"
    ),
    
    # categorical variable 2: Operating System
    h5("Operating System:"),
    checkboxGroupInput(
      inputId = "os_filter",
      label = NULL,
      choices = c("All" = "all",
                  "Android" = "Android",
                  "iOS" = "iOS"),
      selected = "all"
    ),
    
    hr(),
    
    # numeric variable 1: Screen On Time
    h5("Screen On Time (hours/day):"),
    uiOutput("screen_slider_ui"),
    
    # numeric variable 2: Age
    h5("Age:"),
    uiOutput("age_slider_ui"),
    
    hr(),
    
    # action button to apply filters
    actionButton(
      inputId = "apply_filters",
      label = "Apply Filters",
      icon = icon("filter"),
      class = "btn-primary btn-block",
      style = "width: 100%;"
    ),
    
    hr(),
    
    p("Click 'Apply Filters' to update all tabs with the selected subset.",
      style = "font-size: 0.8em; color: #888; font-style: italic;")
  ),
  
  # Main panel with tabs
  tabsetPanel(
    
    # tab 1 -- about tab
    tabPanel(
      title = "About",
      icon = icon("info-circle"),
      
      layout_columns(
        col_widths = c(8, 4),
        
        # app description
        card(
          h2("Mobile Device Usage Explorer"),
          p("This Shiny app allows users to explore the Mobile Device Usage and User Behavior dataset."),
          br(),
          
          h4("Purpose"),
          p("The app provides an interactive interface to investigate patterns in mobile device usage, 
            including app usage time, screen-on time, battery drain, and how these relate to user 
            demographics and behavior classifications."),
          br(),
          
          h4("Data Source"),
          p("The dataset comes from Kaggle and contains 700 user records with 11 variables."),
          p("Data Source:", 
            a("Mobile Device Usage and User Behavior Dataset", 
              href = "https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset",
              target = "_blank")),
          br(),
          
          h4("How to Use This App"),
          tags$ul(
            tags$li(tags$b("Sidebar:"), " Use the filters to subset the data by device model, 
                    operating system, screen time range, and age range."),
            tags$li(tags$b("Apply Filters:"), " Click this button to update all tabs with your selected subset."),
            tags$li(tags$b("Data Download:"), " View and download the (possibly subsetted) data."),
            tags$li(tags$b("Data Exploration:"), " Explore contingency tables, numerical summaries, 
                    and interactive plots.")
          )
        ),
        
        # Image 
        card(
          img(src = "https://www.kaggle.com/static/images/site-logo.png", 
              height = "100px", style = "display: block; margin: 0 auto;"),
          br(),
          p("Dataset from Kaggle", style = "text-align: center; font-style: italic;"),
          br(),
          div(
            style = "background: #f0f8ff; padding: 15px; border-radius: 8px;",
            h5("Dataset Summary", style = "text-align: center;"),
            p(HTML(paste0(
              "<b>Rows:</b> ", nrow(device_usage_data), "<br>",
              "<b>Variables:</b> ", ncol(device_usage_data), "<br>",
              "<b>Behavior Classes:</b> 1 (Light) to 5 (Extreme)"
            )))
          )
        )
      )
    ),
    
    # tab 2 -- data download
    tabPanel(
      title = "Data Download",
      icon = icon("table"),
      
      card(
        h4("Subsetted Data"),
        p("This table shows the data after applying the filters from the sidebar."),
        DTOutput("filtered_table") |> withSpinner(color = "#0dc5c1"),
        br(),
        downloadButton(
          outputId = "download_data",
          label = "Download Data (CSV)",
          class = "btn-success",
          icon = icon("download")
        )
      )
    ),
    
    # tab 3 -- data exploration
    tabPanel(
      title = "Data Exploration",
      icon = icon("chart-bar"),
      
      # Use tabsetPanel inside for subtabs
      tabsetPanel(
        
        # contingency tables tab
        tabPanel(
          title = "Contingency Tables",
          icon = icon("table"),
          
          layout_columns(
            col_widths = c(6, 6),
            
            card(
              h5("One-Way Tables"),
              p("Distribution of categorical variables in the filtered data."),
              tableOutput("oneway_table") |> withSpinner(color = "#0dc5c1")
            ),
            
            card(
              h5("Two-Way Table"),
              p("Select variables to cross-tabulate."),
              selectInput(
                inputId = "twoway_row",
                label = "Row Variable:",
                choices = c("User Behavior Class", "Gender", "Device Model", 
                            "Operating System", "Age_Group", "Screen_Time_Group",
                            "App_Usage_Group", "Battery_Group"),
                selected = "User Behavior Class"
              ),
              selectInput(
                inputId = "twoway_col",
                label = "Column Variable:",
                choices = c("Age_Group", "Gender", "Device Model", 
                            "Operating System", "User Behavior Class", "Screen_Time_Group",
                            "App_Usage_Group", "Battery_Group"),
                selected = "Age_Group"
              ),
              tableOutput("twoway_table") |> withSpinner(color = "#0dc5c1")
            )
          )
        ),