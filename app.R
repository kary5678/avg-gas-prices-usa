#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(maps)
library(readr)
library(dplyr)

# Load gas price data
gas_data <- read_csv("data/gas_prices_texas.csv") |>
  mutate(
    Metro = toupper(Metro),
    Regular = as.numeric(gsub("[$,]", "", Regular)),
    Mid = as.numeric(gsub("[$,]", "", Mid)),
    Premium = as.numeric(gsub("[$,]", "", Premium)),
    Diesel = as.numeric(gsub("[$,]", "", Diesel))
  )

# Hardcoded lat/lon for major cities (simplified)
city_coords <- tibble::tibble(
  Metro = c("AUSTIN", "DALLAS", "HOUSTON", "SAN ANTONIO"),
  lat = c(30.2672, 32.7767, 29.7604, 29.4241),
  lon = c(-97.7431, -96.7970, -95.3698, -98.4936)
)

ui <- fluidPage(
  titlePanel("Texas Gas Prices by City"),
  sidebarLayout(
    sidebarPanel(
      selectInput("fuel_type", "Select Fuel Type",
                  choices = c("Regular", "Mid", "Premium", "Diesel")),
      selectInput("timeframe", "Select Timeframe",
                  choices = c("Current Avg.", "Yesterday Avg.", "Week Ago Avg.",
                              "Month Ago Avg.", "Year Ago Avg."),
                  selected = "Current Avg.")
    ),
    mainPanel(
      plotOutput("mapPlot", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  output$mapPlot <- renderPlot({
    fuel <- input$fuel_type
    timeframe <- input$timeframe
    
    df <- gas_data |>
      filter(Timeframe == timeframe) |>
      inner_join(city_coords, by = "Metro")
    
    map("state", "texas", col = "#cccccc", fill = TRUE)
    points(df$lon, df$lat, pch = 21, bg = "red", cex = 2)
    
    labels <- paste0(df$Metro, ": $", round(df[[fuel]], 2))
    text(df$lon, df$lat, labels = labels, pos = 4, cex = 0.8)
  })
}

shinyApp(ui = ui, server = server)
