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
        
        # numerical summaries tab
        tabPanel(
          title = "Numerical Summaries",
          icon = icon("calculator"),
          
          layout_columns(
            col_widths = c(6, 6),
            
            card(
              h5("Numeric Variable Selection"),
              selectInput(
                inputId = "num_var",
                label = "Select Numeric Variable:",
                choices = c("Age", "Screen On Time (hours/day)", 
                            "App Usage Time (min/day)", "Battery Drain (mAh/day)",
                            "Data Usage (MB/day)", "Number of Apps Installed"),
                selected = "Screen On Time (hours/day)"
              )
            ),
            
            card(
              h5("Categorical Variable for Grouping"),
              selectInput(
                inputId = "group_var",
                label = "Group By:",
                choices = c("User Behavior Class", "Gender", "Device Model", 
                            "Operating System", "Age_Group", "Screen_Time_Group",
                            "App_Usage_Group", "Battery_Group"),
                selected = "User Behavior Class"
              )
            )
          ),
          
          card(
            h5("Summary Statistics"),
            p("Summary statistics of the selected numeric variable across levels of the grouping variable."),
            tableOutput("num_summary") |> withSpinner(color = "#0dc5c1")
          )
        ),
        
        # bar charts tab
        tabPanel(
          title = "Bar Charts",
          icon = icon("chart-bar"),
          
          layout_columns(
            col_widths = c(6, 6),
            
            card(
              h5("Bar Chart Settings"),
              selectInput(
                inputId = "bar_var",
                label = "Variable to Plot:",
                choices = c("User Behavior Class", "Gender", "Device Model", 
                            "Operating System", "Age_Group", "Screen_Time_Group",
                            "App_Usage_Group", "Battery_Group"),
                selected = "User Behavior Class"
              ),
              checkboxInput(
                inputId = "bar_fill",
                label = "Fill by User Behavior Class",
                value = TRUE
              )
            ),
            
            card(
              h5("Bar Chart"),
              plotOutput("bar_plot", height = "400px") |> withSpinner(color = "#0dc5c1")
            )
          )
        ),
        
        # box plots tab
        tabPanel(
          title = "Box Plots",
          icon = icon("box"),
          
          layout_columns(
            col_widths = c(6, 6),
            
            card(
              h5("Box Plot Settings"),
              selectInput(
                inputId = "box_var",
                label = "Numeric Variable:",
                choices = c("Screen On Time (hours/day)", "App Usage Time (min/day)",
                            "Age", "Battery Drain (mAh/day)", "Data Usage (MB/day)"),
                selected = "Screen On Time (hours/day)"
              ),
              selectInput(
                inputId = "box_group",
                label = "Group By:",
                choices = c("User Behavior Class", "Gender", "Device Model", 
                            "Operating System", "Age_Group", "Screen_Time_Group",
                            "App_Usage_Group", "Battery_Group"),
                selected = "User Behavior Class"
              ),
              checkboxInput(
                inputId = "box_facet",
                label = "Facet by Device Model",
                value = FALSE
              )
            ),
            
            card(
              h5("Box Plot"),
              plotOutput("box_plot", height = "400px") |> withSpinner(color = "#0dc5c1")
            )
          )
        ),
        
        # scatter plots tab
        tabPanel(
          title = "Scatter Plots",
          icon = icon("chart-line"),
          
          layout_columns(
            col_widths = c(6, 6),
            
            card(
              h5("Scatter Plot Settings"),
              selectInput(
                inputId = "scatter_x",
                label = "X Variable:",
                choices = c("App Usage Time (min/day)", "Screen On Time (hours/day)",
                            "Age", "Battery Drain (mAh/day)", "Data Usage (MB/day)"),
                selected = "App Usage Time (min/day)"
              ),
              selectInput(
                inputId = "scatter_y",
                label = "Y Variable:",
                choices = c("Battery Drain (mAh/day)", "App Usage Time (min/day)",
                            "Data Usage (MB/day)", "Screen On Time (hours/day)",
                            "Age"),
                selected = "Battery Drain (mAh/day)"
              ),
              checkboxInput(
                inputId = "scatter_color",
                label = "Color by User Behavior Class",
                value = TRUE
              ),
              checkboxInput(
                inputId = "scatter_regression",
                label = "Show Regression Line",
                value = FALSE
              ),
              checkboxInput(
                inputId = "scatter_facet",
                label = "Facet by Device Model",
                value = FALSE
              )
            ),
            
            card(
              h5("Scatter Plot"),
              plotOutput("scatter_plot", height = "400px") |> withSpinner(color = "#0dc5c1")
            )
          )
        ),
        
        # histograms and distribution plots
        tabPanel(
          title = "Distribution Plots",
          icon = icon("chart-area"),
          
          layout_columns(
            col_widths = c(6, 6),
            
            card(
              h5("Distribution Plot Settings"),
              selectInput(
                inputId = "dist_var",
                label = "Numeric Variable:",
                choices = c("Screen On Time (hours/day)", "App Usage Time (min/day)",
                            "Age", "Battery Drain (mAh/day)", "Data Usage (MB/day)"),
                selected = "Screen On Time (hours/day)"
              ),
              selectInput(
                inputId = "dist_type",
                label = "Plot Type:",
                choices = c("Histogram" = "hist", 
                            "Density" = "density",
                            "Violin + Box" = "violin"),
                selected = "hist"
              ),
              checkboxInput(
                inputId = "dist_facet",
                label = "Facet by Device Model",
                value = FALSE
              )
            ),
            
            card(
              h5("Distribution Plot"),
              plotOutput("dist_plot", height = "400px") |> withSpinner(color = "#0dc5c1")
            )
          )
        )
      )
    )
  )
)



# now lets define the server
server <- function(input, output, session) {
  
  # include reactive value
  filtered_data <- reactiveValues(data = device_usage_data)
  
  # observer for 'apply filters' button
  observeEvent(input$apply_filters, {
    
    data_subset <- device_usage_data
    
    # define our data filters
    
    if(!"all" %in% input$device_filter) {
      data_subset <- data_subset |>
        filter(`Device Model` %in% input$device_filter)
    }
    
    if(!"all" %in% input$os_filter) {
      data_subset <- data_subset |>
        filter(`Operating System` %in% input$os_filter)
    }
    
    screen_min <- input$screen_range[1]
    screen_max <- input$screen_range[2]
    data_subset <- data_subset |>
      filter(`Screen On Time (hours/day)` >= screen_min &
               `Screen On Time (hours/day)` <= screen_max)
    
    age_min <- input$age_range[1]
    age_max <- input$age_range[2]
    data_subset <- data_subset |>
      filter(Age >= age_min & Age <= age_max)
    
    # update the reactive value
    filtered_data$data <- data_subset
    
    # show notification
    showNotification(
      paste("Data filtered:", nrow(data_subset), "rows remaining"),
      type = "message",
      duration = 3
    )
  })
  
  # Screen On Time slider
  output$screen_slider_ui <- renderUI({
    sliderInput(
      inputId = "screen_range",
      label = NULL,
      min = 0,
      max = 12,
      value = c(0, 12),
      step = 0.5,
      ticks = FALSE
    )
  })
  
  # Age slider
  output$age_slider_ui <- renderUI({
    sliderInput(
      inputId = "age_range",
      label = NULL,
      min = 18,
      max = 59,
      value = c(18, 59),
      step = 1,
      ticks = FALSE
    )
  })
  
  # downloading the data
  output$filtered_table <- renderDT({
    req(filtered_data$data)
    
    datatable(
      filtered_data$data |>
        select(`User ID`, `Device Model`, `Operating System`, 
               `App Usage Time (min/day)`, `Screen On Time (hours/day)`,
               `Battery Drain (mAh/day)`, `Data Usage (MB/day)`,
               Age, Gender, `User Behavior Class`),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      rownames = FALSE
    )
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("device_usage_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data$data, file, row.names = FALSE)
    }
  )
  
  # contingency tables
  
  # One-way tables
  output$oneway_table <- renderTable({
    req(filtered_data$data)
    data <- filtered_data$data
    
    # summary of one-way tables
    data.frame(
      Variable = c("Gender", "Device Model", "Operating System", 
                   "User Behavior Class", "Age_Group", "Screen_Time_Group"),
      Levels = c(
        paste(names(table(data$Gender)), collapse = ", "),
        paste(names(table(data$`Device Model`)), collapse = ", "),
        paste(names(table(data$`Operating System`)), collapse = ", "),
        paste(names(table(data$`User Behavior Class`)), collapse = ", "),
        paste(names(table(data$Age_Group)), collapse = ", "),
        paste(names(table(data$Screen_Time_Group)), collapse = ", ")
      ),
      Counts = c(
        paste(table(data$Gender), collapse = ", "),
        paste(table(data$`Device Model`), collapse = ", "),
        paste(table(data$`Operating System`), collapse = ", "),
        paste(table(data$`User Behavior Class`), collapse = ", "),
        paste(table(data$Age_Group), collapse = ", "),
        paste(table(data$Screen_Time_Group), collapse = ", ")
      )
    )
  })
  
  # Two-way tables
  output$twoway_table <- renderTable({
    req(filtered_data$data)
    data <- filtered_data$data
    
    table(data[[input$twoway_row]], data[[input$twoway_col]])
  }, rownames = TRUE)
  
  # numerical summaries
  output$num_summary <- renderTable({
    req(filtered_data$data)
    data <- filtered_data$data
    
    # summary statistics with tidyverse
    data |>
      group_by(!!sym(input$group_var))  |>
      summarise(
        n = n(),
        Mean = round(mean(!!sym(input$num_var), na.rm = TRUE), 2),
        Median = round(median(!!sym(input$num_var), na.rm = TRUE), 2),
        SD = round(sd(!!sym(input$num_var), na.rm = TRUE), 2),
        Min = round(min(!!sym(input$num_var), na.rm = TRUE), 2),
        Max = round(max(!!sym(input$num_var), na.rm = TRUE), 2)
      )
  })
  
  # bar charts
  output$bar_plot <- renderPlot({
    req(filtered_data$data)
    data <- filtered_data$data
    
    bar_var_sym <- sym(input$bar_var)
    
    # create the plot
    if(input$bar_fill) {
      p <- ggplot(data, aes(x = !!bar_var_sym, 
                            fill = as.factor(`User Behavior Class`))) +
        geom_bar(position = "dodge") +
        scale_fill_brewer(palette = "Blues", name = "Behavior Class")
    } else {
      p <- ggplot(data, aes(x = !!bar_var_sym)) +
        geom_bar(fill = "steelblue", color = "white", alpha = 0.8)
    }
    
    p <- p +
      labs(title = paste("Distribution of", input$bar_var),
           x = input$bar_var,
           y = "Count") +
      theme_minimal()
    p
  })
  
  # Box plot
  output$box_plot <- renderPlot({
    req(filtered_data$data)
    data <- filtered_data$data
    
    box_var_sym <- sym(input$box_var)
    box_group_sym <- sym(input$box_group)
    
    # Convert grouping variable to factors
    p <- ggplot(data, aes(x = as.factor(!!box_group_sym), 
                          y = !!box_var_sym,
                          fill = as.factor(!!box_group_sym))) +
      geom_boxplot() +
      scale_fill_brewer(palette = "Blues") +
      labs(title = paste(input$box_var, "by", input$box_group),
           x = input$box_group,
           y = input$box_var) +
      theme_minimal()
    
    if(input$box_facet) {
      p <- p + facet_wrap(~`Device Model`, ncol = 3)
    }
    
    p
  })
  
  # Scatter plots
  output$scatter_plot <- renderPlot({
    req(filtered_data$data)
    data <- filtered_data$data
    
    scatter_x_sym <- sym(input$scatter_x)
    scatter_y_sym <- sym(input$scatter_y)
    
    # Base plot without color
    if(input$scatter_color) {
      # Convert User Behavior Class to factor for discrete color scale
      p <- ggplot(data, aes(x = !!scatter_x_sym, 
                            y = !!scatter_y_sym, 
                            color = as.factor(`User Behavior Class`))) +
        geom_point(alpha = 0.7, size = 2) +
        scale_color_brewer(palette = "Dark2", name = "Behavior Class")
    } else {
      p <- ggplot(data, aes(x = !!scatter_x_sym, y = !!scatter_y_sym)) +
        geom_point(color = "steelblue", alpha = 0.6, size = 2)
    }
    
    if(input$scatter_regression) {
      p <- p + geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.2)
    }
    
    p <- p +
      labs(title = paste(input$scatter_x, "vs", input$scatter_y),
           x = input$scatter_x,
           y = input$scatter_y) +
      theme_minimal()
    
    if(input$scatter_facet) {
      p <- p + facet_wrap(~`Device Model`, ncol = 3)
    }
    
    p
  })