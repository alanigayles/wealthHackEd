# Load necessary libraries
library(ggplot2)
library(shiny)
library(lubridate)

# Read the CSV file
data <- read.csv("2022_transactions.csv")

# Convert Date column to Date format
data$Date <- as.Date(data$Date)

# Define UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("time_duration", "Time Duration:", 
                  choices = c("One Month", "Three Months", "Six Months", 
                              "Nine Months", "One Year", "Three Years", 
                              "All Time", "Custom"),
                  selected = "One Month"),
      conditionalPanel(
        condition = "input.time_duration == 'Custom'",
        dateInput("start_date", "Start Date:"),
        dateInput("end_date", "End Date:")
      )
    ),
    mainPanel(
      plotOutput("transaction_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Prepare data based on user input
  filtered_data <- reactive({
    time_duration <- input$time_duration
    
    if (time_duration == "Custom") {
      start_date <- input$start_date
      end_date <- input$end_date
      subset(data, Date >= start_date & Date <= end_date)
    } else if (time_duration == "One Month") {
      cutoff_date <- Sys.Date() - as.period(1, "months")
      subset(data, Date > cutoff_date)
    } else if (time_duration == "Three Months") {
      cutoff_date <- Sys.Date() - as.period(3, "months")
      subset(data, Date > cutoff_date)
    } else if (time_duration == "Six Months") {
      cutoff_date <- Sys.Date() - as.period(6, "months")
      subset(data, Date > cutoff_date)
    } else if (time_duration == "Nine Months") {
      cutoff_date <- Sys.Date() - as.period(9, "months")
      subset(data, Date > cutoff_date)
    } else if (time_duration == "One Year") {
      cutoff_date <- Sys.Date() - as.period(1, "years")
      subset(data, Date > cutoff_date)
    } else if (time_duration == "Three Years") {
      cutoff_date <- Sys.Date() - as.period(3, "years")
      subset(data, Date > cutoff_date)
    } else if (time_duration == "All Time") {
      data
    }
  })
  
  # Render the plot
  output$transaction_plot <- renderPlot({
    graph <- ggplot(filtered_data(), aes(x = format(Date, "%b"), y = Amount)) +
      geom_bar(stat = "identity", fill = "green", alpha = 0.7) +
      labs(x = "Month", y = "Amount ($)", title = "Comparison of Time Durations") +
      theme_minimal()
    
    print(graph)
  })
}

# Run the app
shinyApp(ui, server)