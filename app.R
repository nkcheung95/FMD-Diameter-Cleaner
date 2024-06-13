#DIA CLEAN APP
# Define the packages you want to use
library(shiny)
packages <- c("tidyverse", "zoo", "scales", "ggplot2","shiny")

# Function to install and load packages
install_load_packages <- function(packages) {
  # Check which packages are not installed
  not_installed <- setdiff(packages, rownames(installed.packages()))
  
  # Install the missing packages
  if (length(not_installed) > 0) {
    install.packages(not_installed)
  }
  
  # Load all the packages
  invisible(sapply(packages, library, character.only = TRUE))
}

# Call the function to install and load packages
install_load_packages(packages)

# Define UI
ui <- fluidPage(
  titlePanel("FMD DIAMETER CLEANER"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose TXT File", 
                accept = c("text/plain", ".txt")),
      selectInput("delimiter", "Delimiter", 
                  choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = "\t"),
      uiOutput("column_selectors"),
      sliderInput("window_size", "Window Size (Odd Numbers Only)", 
                  min = 11, max = 201, value = 11, step = 2, ticks = FALSE),
      sliderInput("threshold", "Threshold", 
                  min = 0, max = 3, value = 1.5, step = 0.5),
      downloadButton("download_data", "Download Cleaned Data")
    ),
    mainPanel(
      verbatimTextOutput("file_info"),
      plotOutput("raw_plot"),
      plotOutput("clean_plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive expression to read the uploaded file
  data <- reactive({
    req(input$file1)
    read.delim(input$file1$datapath, sep = input$delimiter, header = FALSE)
  })
  
  # Reactive expression to trim the first 8 rows
  trimmed_data <- reactive({
    req(data())
    data()[-(1:8), ]
  })
  
  # Dynamically generate UI for column selection based on uploaded file
  output$column_selectors <- renderUI({
    req(trimmed_data())
    raw_data <- trimmed_data()
    col_names <- colnames(raw_data)
    
    tagList(
      selectInput("time_col", "Select Time Column", choices = col_names, selected = "V1"),
      selectInput("diameter_col", "Select Diameter Column", choices = col_names, selected = "V2"),
      selectInput("comments_col", "Select Comments Column (optional)", choices = c("None", col_names), selected = "V3")
    )
  })
  
  # Output file info for debugging
  output$file_info <- renderPrint({
    df <- trimmed_data()
    str(df)
  })
  
  # Plot the raw data
  output$raw_plot <- renderPlot({
    req(trimmed_data(), input$time_col, input$diameter_col)
    raw_data <- trimmed_data()
    
    # Rename columns based on user selection
    colnames(raw_data)[colnames(raw_data) == input$time_col] <- "time"
    colnames(raw_data)[colnames(raw_data) == input$diameter_col] <- "diameter"
    if (input$comments_col != "None") {
      colnames(raw_data)[colnames(raw_data) == input$comments_col] <- "comments"
    }
    
    raw_data$diameter <- as.numeric(as.character(raw_data$diameter))
    raw_data$time <- as.numeric(raw_data$time) - min(as.numeric(raw_data$time))  # Start time at zero
    
    # Generate raw_plot
    ggplot(raw_data, aes(x = time, y = diameter)) +
      geom_point() +
      geom_vline(xintercept = c(60, 360), color = "red", linetype = "dashed") +  # Add vertical lines
      scale_x_continuous(breaks = seq(0, max(raw_data$time, na.rm = TRUE), by = 30), labels = comma) +
      labs(x = "Time (seconds)", y = "Diameter", title = "Raw Data Plot") +
      theme_minimal()
  })
  
  # Plot the cleaned data
  output$clean_plot <- renderPlot({
    req(trimmed_data(), input$time_col, input$diameter_col)
    raw_data <- trimmed_data()
    
    # Rename columns based on user selection
    colnames(raw_data)[colnames(raw_data) == input$time_col] <- "time"
    colnames(raw_data)[colnames(raw_data) == input$diameter_col] <- "diameter"
    if (input$comments_col != "None") {
      colnames(raw_data)[colnames(raw_data) == input$comments_col] <- "comments"
    }
    
    raw_data$diameter <- as.numeric(as.character(raw_data$diameter))
    raw_data$time <- as.numeric(raw_data$time) - min(as.numeric(raw_data$time))  # Start time at zero
    
    # Cleaning and interpolation steps
    window_size <- input$window_size
    
    raw_data$moving_median <- rollmedian(raw_data$diameter, window_size, align = "center", fill = NA)
    iqr <- IQR(raw_data$moving_median, na.rm = TRUE)
    threshold <- input$threshold * iqr  # Dynamic threshold based on IQR of the moving median
    raw_data$outlier <- abs(raw_data$diameter - raw_data$moving_median) > threshold
    
    raw_data$interp_diameter <- raw_data$diameter  # Create a new column for interpolated values
    raw_data$interp_diameter[raw_data$outlier | is.na(raw_data$diameter)] <- NA  # Set outliers and NA values to NA
    raw_data$interp_diameter <- na.approx(raw_data$interp_diameter)  # Interpolate NA values
    
    cleaned_data <- raw_data[!is.na(raw_data$interp_diameter), ]  # Remove rows with NA interp_diameter
    
    # Generate clean_plot
    ggplot(cleaned_data, aes(x = time, y = interp_diameter, color = outlier)) +
      geom_point() +
      geom_vline(xintercept = c(60, 360), color = "red", linetype = "dashed") +  # Add vertical lines
      scale_x_continuous(breaks = seq(0, max(cleaned_data$time, na.rm = TRUE), by = 30), labels = comma) +
      labs(x = "Time (seconds)", y = "Interpolated Diameter", title = "Cleaned Data Plot",color = "Interpolated") +
      theme_minimal() +
      theme(legend.position = c(0.8, 0.2))  # Move legend inside plot area
  })
  
  # Download handler for cleaned data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("cleaned_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      raw_data <- trimmed_data()
      
      # Rename columns based on user selection
      colnames(raw_data)[colnames(raw_data) == input$time_col] <- "time"
      colnames(raw_data)[colnames(raw_data) == input$diameter_col] <- "diameter"
      if (input$comments_col != "None") {
        colnames(raw_data)[colnames(raw_data) == input$comments_col] <- "comments"
      }
      
      raw_data$diameter <- as.numeric(as.character(raw_data$diameter))
      raw_data$time <- as.numeric(raw_data$time) - min(as.numeric(raw_data$time))  # Start time at zero
      
      # Cleaning and interpolation steps
      window_size <- input$window_size
      
      raw_data$moving_median <- rollmedian(raw_data$diameter, window_size, align = "center", fill = NA)
      iqr <- IQR(raw_data$moving_median, na.rm = TRUE)
      threshold <- input$threshold * iqr  # Dynamic threshold based on IQR of the moving median
      raw_data$outlier <- abs(raw_data$diameter - raw_data$moving_median) > threshold
      
      raw_data$interp_diameter <- raw_data$diameter  # Create a new column for interpolated values
      raw_data$interp_diameter[raw_data$outlier | is.na(raw_data$diameter)] <- NA  # Set outliers and NA values to NA
      raw_data$interp_diameter <- na.approx(raw_data$interp_diameter)  # Interpolate NA values
      
      cleaned_data <- raw_data[!is.na(raw_data$interp_diameter), ]  # Remove rows with NA interp_diameter
      
      write.csv(cleaned_data, file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

