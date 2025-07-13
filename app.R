#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(RColorBrewer)
library(tigris)

# Load city shapes once
tx_cities <- places(state = "TX", year = 2022, class = "sf")

# Load and clean gas price data
gas_data <- read_csv("data/gas_prices_texas.csv") |>
  #subset(Timeframe == "Current Avg.") |>
  mutate(
    Metro = toupper(Metro),
    Regular = as.numeric(gsub("[$,]", "", Regular)),
    Mid = as.numeric(gsub("[$,]", "", Mid)),
    Premium = as.numeric(gsub("[$,]", "", Premium)),
    Diesel = as.numeric(gsub("[$,]", "", Diesel))
  )

# Preprocess geometry merge
tx_map_data <- tx_cities |>
  mutate(NAME = toupper(NAME)) |>
  inner_join(gas_data, by = c("NAME" = "Metro"))

# Start the app
ui <- fluidPage(
  titlePanel("Texas Gas Prices by City"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "fuel_type",
        label = "Select Fuel Type",
        choices = c("Regular", "Mid", "Premium", "Diesel"),
        selected = "Regular"
      ),
      selectInput(
        inputId = "timeframe",
        label = "Select Timeframe",
        choices = c("Current Avg.", "Yesterday Avg.", "Week Ago Avg.", "Month Ago Avg.", "Year Ago Avg."),
        selected = "Current Avg."
      )
    ),
    mainPanel(
      leafletOutput("gas_map", height = "700px")
    )
  )
)

# Define server logic required to draw plots
server <- function(input, output, session) {
  # Reactive data filtered by timeframe
  filtered_data <- reactive({
    # Filter gas_data to selected timeframe
    df <- gas_data %>%
      filter(Timeframe == input$timeframe)
    
    # Rejoin with city geometries
    tx_cities %>%
      mutate(NAME = toupper(NAME)) %>%
      inner_join(df %>% mutate(Metro = toupper(Metro)), by = c("NAME" = "Metro"))
  })
  
  output$gas_map <- renderLeaflet({
    fuel <- input$fuel_type
    map_data <- filtered_data()
    palette_vals <- map_data[[fuel]]
    pal <- colorNumeric("YlOrRd", domain = palette_vals)
    
    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(map_data[[fuel]]),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = ~paste0(NAME, ": $", round(map_data[[fuel]], 3)),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal,
        values = palette_vals,
        title = paste("Avg Price ($)", input$timeframe),
        position = "bottomright"
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
