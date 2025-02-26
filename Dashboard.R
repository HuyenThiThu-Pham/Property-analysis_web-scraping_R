# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(broom)
library(corrplot)
library(shinydashboard)


# Load your filtered dataset
load("data_filtered.RData")
data_model <- data_filtered %>% 
  dplyr::select(ID, price, bedrooms, bathrooms, car, land_size, year)

# Ensure price is numeric
data_model$price <- as.numeric(data_model$price)

# Split data into training and testing sets
set.seed(42)
trainIndex <- sample(seq_len(nrow(data_model)), size = 0.8 * nrow(data_model))
data_train <- data_model[trainIndex, ]
data_test <- data_model[-trainIndex, ]

# Linear Regression Model
lr_model <- lm(price ~ bedrooms + bathrooms + car + land_size + year, data = data_train)

#---------------------------------

# Define the UI for the Shiny dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Prospect Real Estate Dashboard 2014-2024"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      # Overview tab
      tabItem(
        tabName = "overview",
        fluidRow(
          box(width = 4,
              numericInput("bedrooms", "Bedrooms:", min = 0, value = 3),
              numericInput("bathrooms", "Bathrooms:", min = 0, value = 2),
              numericInput("car", "Car Spaces:", min = 0, value = 2),
              numericInput("land_size", "Land Size (sqm):", min = 0, value = 500),
              numericInput("year", "Year of Sale:", min = 2014, max = 2024, value = 2020),
              actionButton("predict", "Predict Price"),
              verbatimTextOutput("prediction_result")
          ),
          box(width = 8, plotOutput("price_distribution"))
        ),
        fluidRow(
          valueBoxOutput("avg_price"),
          valueBoxOutput("total_sales"),
          valueBoxOutput("popular_type")
        )
      ),
      # Analysis tab
      tabItem(
        tabName = "analysis",
        fluidRow(
          box(width = 6, plotOutput("price_trend_plot")),
          box(width = 6, plotOutput("monthly_sales_plot"))
        ),
        fluidRow(
          box(width = 6, plotOutput("feature_importance")),
          box(width = 6, plotOutput("top_streets"))
        )
      )
    )
  )
)

#----------------------------------

# Define the server logic
server <- function(input, output, session) {
  # Top Insights
  output$avg_price <- renderValueBox({
    avg_price <- mean(data_model$price, na.rm = TRUE)
    valueBox(value = paste0("$", format(round(avg_price, 2), nsmall = 2)), "Average Price", icon = icon("dollar"))
  })
  
  output$total_sales <- renderValueBox({
    total_sales <- nrow(data_model)
    valueBox(value = total_sales, "Total Properties Sold", icon = icon("home"))
  })
  
  output$popular_type <- renderValueBox({
    popular_type <- data_filtered %>% 
      count(type) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(type)
    valueBox(value = popular_type, "Most Common Property Type", icon = icon("building"))
  })
  
  # Price Distribution
  output$price_distribution <- renderPlot({
    ggplot(data_filtered, aes(x = price)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "skyblue", alpha = 0.7) +
      geom_density(color = "red") +
      labs(title = "Distribution of Property Prices (2014-2024)", x = "Price", y = "Density") +
      theme_minimal()
  })
  
  # Price Trends Over Time
  output$price_trend_plot <- renderPlot({
    price_trends <- data_filtered %>%
      group_by(year) %>%
      summarise(avg_price = mean(price, na.rm = TRUE), total_sales = n(), .groups = "drop")
    
    ggplot(price_trends, aes(x = year)) +
      geom_line(aes(y = avg_price), color = "blue") +
      geom_bar(aes(y = total_sales * max(avg_price) / max(total_sales)), stat = "identity", fill = "lightgrey", alpha = 0.5) +
      labs(title = "Price Trends Over Time", y = "Average Price", x = "Year") +
      theme_minimal()
  })
  
  # Monthly Sales Trends
  output$monthly_sales_plot <- renderPlot({
    monthly_sales <- data_filtered %>%
      group_by(month) %>%
      summarise(total_sales = n(), .groups = "drop")
    
    ggplot(monthly_sales, aes(x = month, y = total_sales)) +
      geom_line(group = 1, color = "blue", linewidth = 1) +
      geom_point(color = "darkred", size = 2) +
      labs(
        title = "Seasonal Property Sales Trends",
        x = "Month",
        y = "Total Sales"
      ) +
      theme_minimal()
  })
  
  # Feature Importance
  output$feature_importance <- renderPlot({
    coefs <- broom::tidy(lr_model) %>%
      filter(term != "(Intercept)") %>%
      mutate(term = reorder(term, estimate))
    
    ggplot(coefs, aes(x = term, y = estimate)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      coord_flip() +
      labs(title = "Feature Importance (Linear Regression Coefficients)", x = "Feature", y = "Coefficient") +
      theme_minimal()
  })
  
  # Price Distribution by Top Streets
  output$top_streets <- renderPlot({
    top_streets <- data_filtered %>%
      count(street, sort = TRUE) %>%
      top_n(10, wt = n) %>%
      pull(street)
    
    ggplot(data_filtered %>% filter(street %in% top_streets), aes(x = reorder(street, price, FUN = median), y = price)) +
      geom_boxplot(fill = "lightgreen", color = "darkgreen") +
      coord_flip() +
      labs(title = "Top Streets by Frequency", x = "Street", y = "Price") +
      theme_minimal()
  })
  
  # Prediction Logic
  observeEvent(input$predict, {
    new_data <- data.frame(
      bedrooms = input$bedrooms,
      bathrooms = input$bathrooms,
      car = input$car,
      land_size = input$land_size,
      year = input$year
    )
    
    predicted_price <- predict(lr_model, new_data)
    
    output$prediction_result <- renderText({
      paste("Predicted Price: $", round(predicted_price, 2))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
