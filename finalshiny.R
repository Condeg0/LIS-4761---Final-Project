library(shiny)
library(tidyverse)

# Load the cleaned merged data
merged_data <- na.omit(merged_data)

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
    
    # No need to filter by sentiment sign anymore
    data
  })
  
  selected_sentiment <- reactive({
    if (input$sentiment_type == "Positive") {
      return("Positive")
    } else if (input$sentiment_type == "Negative") {
      return("Negative")
    } else {
      return("MeanSentiment")  # Default
    }
  })
  
  # Scaling function to properly display everything
  scaling_factor <- reactive({
    if (input$sentiment_type == "Positive") {
      return(1/100)
    } else if (input$sentiment_type == "Negative") {
      return(1/10)
    } else {
      return(10)  # No scaling for MeanSentiment
    }
  })
  
  output$sentimentPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = Date, y = .data[[selected_sentiment()]])) +
      geom_line(color = "darkgreen") +
      labs(title = "Sentiment Over Time",
           x = "Date",
           y = "Sentiment Score")
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
      geom_line(aes(y = .data[[selected_sentiment()]] * scaling_factor()), color = "red") +
      labs(title = "NVDA Stock Prices and Scaled Sentiment",
           x = "Date",
           y = "Value",
           caption = "Blue = Stock Price, Red = Scaled Sentiment")
  })
}

# Run the App
shinyApp(ui = ui, server = server)
