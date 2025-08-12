install.packages(c("shiny","tidyverse","plotly","leaflet","lubridate",
                   "prophet","shinyWidgets","bslib","scales","leaflet.extras2"))

install.packages("rsconnect")
library(rsconnect)


library(shiny)
library(tidyverse)
library(plotly)
library(leaflet)
library(lubridate)

# Load datasets (update paths if needed)
confirmed <- read.csv("time_series_covid19_confirmed_global.csv")
deaths <- read.csv("time_series_covid19_deaths_global.csv")
recovered <- read.csv("time_series_covid19_recovered_global.csv")

# Function to pivot and tidy each dataset
tidy_covid_data <- function(df, value_name) {
  df %>%
    pivot_longer(
      cols = starts_with("X"),
      names_to = "Date",
      values_to = value_name
    ) %>%
    mutate(Date = as.Date(sub("X", "", Date), format = "%m.%d.%y"))
}

confirmed_long <- tidy_covid_data(confirmed, "Confirmed")
deaths_long <- tidy_covid_data(deaths, "Deaths")
recovered_long <- tidy_covid_data(recovered, "Recovered")

# Join datasets into one dataframe
covid_data <- confirmed_long %>%
  left_join(deaths_long %>% select(-Lat, -Long), by = c("Province.State", "Country.Region", "Date")) %>%
  left_join(recovered_long %>% select(-Lat, -Long), by = c("Province.State", "Country.Region", "Date")) %>%
  mutate(
    Confirmed = replace_na(Confirmed, 0),
    Deaths = replace_na(Deaths, 0),
    Recovered = replace_na(Recovered, 0)
  )

# UI
countries <- sort(unique(covid_data$Country.Region))
date_min <- min(covid_data$Date)
date_max <- max(covid_data$Date)

ui <- fluidPage(
  titlePanel("COVID-19 Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", choices = countries, selected = "South Africa"),
      dateRangeInput("dateRange", "Select Date Range:", start = date_min, end = date_max,
                     min = date_min, max = date_max),
      # Added summary statistics panel
      wellPanel(
        h4("Summary Statistics"),
        hr(),
        h5("Confirmed Cases:"),
        textOutput("confirmedStats"),
        h5("Deaths:"),
        textOutput("deathStats"),
        h5("Recovered Cases:"),
        textOutput("recoveredStats")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Confirmed Cases", plotlyOutput("confirmedPlot")),
        tabPanel("Deaths", plotlyOutput("deathsPlot")),
        tabPanel("Recovered", plotlyOutput("recoveredPlot")),
        tabPanel("Map", leafletOutput("covidMap", height = 600))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    covid_data %>%
      filter(
        Country.Region == input$country,
        Date >= input$dateRange[1],
        Date <= input$dateRange[2]
      ) %>%
      group_by(Date) %>%
      summarise(
        Confirmed = sum(Confirmed),
        Deaths = sum(Deaths),
        Recovered = sum(Recovered)
      ) %>%
      arrange(Date)
  })
  
  # Summary statistics outputs
  output$confirmedStats <- renderText({
    data <- filtered_data()
    paste("Highest:", max(data$Confirmed, na.rm = TRUE))
  })
  
  output$deathStats <- renderText({
    data <- filtered_data()
    paste("Highest:", max(data$Deaths, na.rm = TRUE))
  })
  
  output$recoveredStats <- renderText({
    data <- filtered_data()
    paste("Highest:", max(data$Recovered, na.rm = TRUE))
  })
  
  
  output$confirmedPlot <- renderPlotly({
    data <- filtered_data()
    plot_ly(data, x = ~Date, y = ~Confirmed, type = 'scatter', mode = 'lines+markers',
            line = list(color = 'blue')) %>%
      layout(title = paste("Confirmed Cases in", input$country),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Cases"))
  })
  
  output$deathsPlot <- renderPlotly({
    data <- filtered_data()
    plot_ly(data, x = ~Date, y = ~Deaths, type = 'scatter', mode = 'lines+markers',
            line = list(color = 'red')) %>%
      layout(title = paste("Deaths in", input$country),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Deaths"))
  })
  
  output$recoveredPlot <- renderPlotly({
    data <- filtered_data()
    plot_ly(data, x = ~Date, y = ~Recovered, type = 'scatter', mode = 'lines+markers',
            line = list(color = 'green')) %>%
      layout(title = paste("Recovered Cases in", input$country),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Recovered"))
  })
  
  # Map data reactive for latest date in selected range
  map_data <- reactive({
    latest_date <- max(filter(covid_data, Country.Region == input$country &
                                Date >= input$dateRange[1] & Date <= input$dateRange[2])$Date)
    covid_data %>%
      filter(Country.Region == input$country, Date == latest_date) %>%
      group_by(Province.State, Lat, Long) %>%
      summarise(
        Confirmed = sum(Confirmed),
        Deaths = sum(Deaths),
        Recovered = sum(Recovered)
      )
  })
  
  output$covidMap <- renderLeaflet({
    data <- map_data()
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Long,
        lat = ~Lat,
        radius = ~sqrt(Confirmed) / 5,
        color = "blue",
        stroke = FALSE,
        fillOpacity = 0.5,
        popup = ~paste0(
          "<strong>", Province.State, "</strong><br>",
          "Confirmed: ", Confirmed, "<br>",
          "Deaths: ", Deaths, "<br>",
          "Recovered: ", Recovered
        )
      ) %>%
      setView(lng = mean(data$Long, na.rm = TRUE), lat = mean(data$Lat, na.rm = TRUE), zoom = 4)
  })
}

# Run the app
shinyApp(ui = ui, server = server)


rsconnect::setAccountInfo(name='lwazoluhle',
                          token='E20F39A7DBF893D6DBFF63757BBF0148',
                          secret='<SECRET>')

rsconnect::deployApp("path/to/your/app/folder")


