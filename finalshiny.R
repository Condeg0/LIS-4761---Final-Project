library(shiny)
library(tidyverse)

# Load the cleaned merged data
merged_data <- read_csv("merged_data_final.csv")

# Convert Date column properly
merged_data$Date <- as.Date(merged_data$Date)

# Define UI
ui <- fluidPage(
  titlePanel("Nvidia (NVDA) Stock and Sentiment Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", 
                     "Select Date Range:", 
                     start = min(merged_data$Date), 
                     end = max(merged_data$Date)),
      selectInput("sentiment_type", 
                  "Select Sentiment Type:",
                  choices = c("All", "Positive", "Negative"),
                  selected = "All")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Sentiment Over Time", plotOutput("sentimentPlot")),
        tabPanel("Stock Price Over Time", plotOutput("stockPlot")),
        tabPanel("Combined View", plotOutput("combinedPlot"))
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  filtered_data <- reactive({
    data <- merged_data %>%
      filter(Date >= input$date_range[1], Date <= input$date_range[2])
    
    if (input$sentiment_type == "Positive") {
      data <- data %>% filter(avg_sentiment > 0)
    } else if (input$sentiment_type == "Negative") {
      data <- data %>% filter(avg_sentiment < 0)
    }
    
    data
  })
  
  output$sentimentPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Date, y = avg_sentiment)) +
      geom_line(color = "darkgreen") +
      labs(title = "Average Sentiment Over Time",
           x = "Date",
           y = "Average Sentiment Score")
  })
  
  output$stockPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Date, y = NVDA.Close)) +
      geom_line(color = "steelblue") +
      labs(title = "Nvidia (NVDA) Closing Stock Prices",
           x = "Date",
           y = "Closing Price (USD)")
  })
  
  output$combinedPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Date)) +
      geom_line(aes(y = NVDA.Close), color = "blue") +
      geom_line(aes(y = avg_sentiment * 10), color = "red") +
      labs(title = "NVDA Stock Prices and Scaled Sentiment",
           x = "Date",
           y = "Value",
           caption = "Blue = Stock Price, Red = Scaled Sentiment")
  })
}

# Run the App
shinyApp(ui = ui, server = server)