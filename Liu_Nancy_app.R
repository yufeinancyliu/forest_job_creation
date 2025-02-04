# Load required libraries
library(shiny)   # Make sure shiny is loaded
library(DT)      # For data table
library(ggplot2) # For plotting
library(dplyr)   # For data manipulation
library(lubridate) # For date manipulation

# Read in the final_data (for first task)
final_data <- read.csv("final_data.csv")  # Replace with the correct path to your final_data.csv

# Read in the insurance_data_long (for second task)
insurance_data_long <- read.csv("insurance_data_long.csv")  # Replace with the correct path to your insurance_data_long.csv

# Create a mapping table for the industry based on the provided NAICS code list
naics_mapping <- data.frame(
  naics_code = c("11", "21", "23", "31-33", "42", "44-45", "48-49", "22", "51", 
                 "52", "53", "54", "55", "56", "61", "62", "71", "72", "81", "92"),
  industry = c("Agriculture, forestry, fishing and hunting",
               "Mining, quarrying, and oil and gas extraction",
               "Construction",
               "Manufacturing",
               "Wholesale trade",
               "Retail trade",
               "Transportation and warehousing",
               "Utilities",
               "Information",
               "Finance and insurance",
               "Real estate and rental and leasing",
               "Professional, scientific, and technical services",
               "Management of companies and enterprises",
               "Administrative and support and waste management services",
               "Educational services",
               "Health care and social assistance",
               "Arts, entertainment, and recreation",
               "Accommodation and food services",
               "Other services, except public administration",
               "Public administration")
)

# Define the UI
ui <- fluidPage(
  titlePanel("Unemployment and Employment Dashboard"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Input: Select a state for final_data
      selectInput("state", "Select State:", choices = unique(final_data$state)),
      
      # Input: Select NAICS code for insurance_data_long
      selectInput("naics_code", "Select NAICS 2-digit Industry Code:", 
                  choices = unique(insurance_data_long$naics_code), selected = "11")
    ),
    
    # Main panel to show the table and optionally the chart
    mainPanel(
      DTOutput("data_table"),  # Display data table for final_data
      br(),
      textOutput("industry_name"), # Display industry name for selected NAICS code
      plotOutput("line_chart") # Display line chart for insurance_data_long
    )
  )
)

# Define the server function
server <- function(input, output) {
  
  # Filter data based on selected state for final_data and date range (July 2019 to July 2020)
  filtered_data <- reactive({
    final_data %>%
      filter(state == input$state) %>%
      filter(date >= "2019-07-01" & date <= "2020-07-31")
  })
  
  # Render data table for final_data
  output$data_table <- renderDT({
    filtered_data()
  })
  
  # Render the industry name based on selected NAICS code for insurance_data_long
  output$industry_name <- renderText({
    selected_industry <- naics_mapping %>%
      filter(naics_code == input$naics_code) %>%
      pull(industry)
    paste("Selected Industry: ", selected_industry)
  })
  
  # Define the TNC brand colors
  tnc_colors <- list(
    leaf_green = "#49a942",
    pollen_yellow = "#ffc700",
    tropic_blue = "#01c1e3",
    coral_red = "#ff004d",
    benthic_blue = "#01002a",
    pine_green = "#00291f",
    earth_brown = "#381300",
    mist_white = "#eae8f5"
  )
  
  # Render line chart for the selected NAICS code using insurance_data_long and date range filter
  output$line_chart <- renderPlot({
    # Filter insurance data for selected NAICS code and date range (July 2019 to July 2020)
    chart_data <- insurance_data_long %>%
      filter(naics_code == input$naics_code) %>%
      filter(date >= "2019-07-01" & date <= "2020-07-31")
    
    # Convert the date to year-month format
    chart_data$date <- as.Date(chart_data$date, format = "%b%Y") # Assuming the date is in 'Jan2019' format
    chart_data$year_month <- format(chart_data$date, "%Y-%m")
    
    # Plot the data with year-month on the x-axis using TNC brand colors
    ggplot(chart_data, aes(x = year_month, y = naics_value, group = 1)) +
      geom_line(color = tnc_colors$tropic_blue) + # Use Tropic Blue for the line
      geom_point(color = tnc_colors$leaf_green, size = 2) + # Use Leaf Green for points
      labs(title = paste("Unemployment Insurance Filings for NAICS Code", input$naics_code),
           x = "Year-Month", y = "Unemployment Insurance Filings") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  })
}

# Run the application
shinyApp(ui = ui, server = server)
