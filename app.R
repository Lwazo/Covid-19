# Install & Load Packages
packages <- c("shiny","tidyverse","plotly","leaflet","lubridate","scales","shinyWidgets","bslib")
install.packages(setdiff(packages, rownames(installed.packages())))

# Load libraries
library(shiny)
library(tidyverse)   # includes dplyr, tidyr, ggplot2
library(plotly)
library(leaflet)
library(lubridate)
library(scales)
library(shinyWidgets)
library(bslib)

# Files
data_path <- "data/"

confirmed <- read.csv(paste0(data_path, "time_series_covid19_confirmed_global.csv"))
deaths    <- read.csv(paste0(data_path, "time_series_covid19_deaths_global.csv"))
recovered <- read.csv(paste0(data_path, "time_series_covid19_recovered_global.csv"))


# Tidy Data Function
tidy_covid_data <- function(df, value_name){
  df %>%
    pivot_longer(
      cols = matches("^[0-9]|^X"),
      names_to = "Date",
      values_to = value_name
    ) %>%
    mutate(Date = as.Date(gsub("^X","",Date), format="%m.%d.%y"))
}

confirmed_long <- tidy_covid_data(confirmed, "Confirmed")
deaths_long    <- tidy_covid_data(deaths, "Deaths")
recovered_long <- tidy_covid_data(recovered, "Recovered")

# Combine datasets
covid_data <- confirmed_long %>%
  left_join(deaths_long %>% select(-Lat,-Long), by=c("Province.State","Country.Region","Date")) %>%
  left_join(recovered_long %>% select(-Lat,-Long), by=c("Province.State","Country.Region","Date")) %>%
  mutate(
    Confirmed = replace_na(Confirmed,0),
    Deaths = replace_na(Deaths,0),
    Recovered = replace_na(Recovered,0),
    MortalityRate = ifelse(Confirmed==0,0, round(Deaths/Confirmed*100,2))
  )

# UI

countries <- sort(unique(covid_data$Country.Region))
date_min <- min(covid_data$Date)
date_max <- max(covid_data$Date)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly"),
  titlePanel("🌍 COVID-19 Global Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        inputId = "country",
        label = "Select Country:",
        choices = countries,
        selected = "South Africa",
        multiple = FALSE,
        options = pickerOptions(liveSearch = TRUE)
      ),
      sliderInput(
        "dateRange",
        "Select Date Range:",
        min = date_min,
        max = date_max,
        value = c(date_min, date_max),
        timeFormat = "%Y-%m-%d"
      ),
      hr(),
      h4("Summary Statistics"),
      uiOutput("summaryBoxes")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Confirmed Cases", plotlyOutput("confirmedPlot")),
        tabPanel("Deaths", plotlyOutput("deathsPlot")),
        tabPanel("Recovered", plotlyOutput("recoveredPlot")),
        tabPanel("Map", leafletOutput("covidMap", height=600))
      )
    )
  )
)

# Server

server <- function(input, output, session){
  
  # Filter data for selected country and date range
  filtered_data <- reactive({
    covid_data %>%
      filter(Country.Region == input$country,
             Date >= input$dateRange[1],
             Date <= input$dateRange[2]) %>%
      group_by(Date) %>%
      summarise(
        Confirmed = sum(Confirmed),
        Deaths = sum(Deaths),
        Recovered = sum(Recovered),
        MortalityRate = ifelse(Confirmed==0,0, round(Deaths/Confirmed*100,2)),
        .groups = "drop"
      )
  })
  
  # Summary Boxes
  output$summaryBoxes <- renderUI({
    data <- filtered_data()
    
    tagList(
      fluidRow(
        column(3, div(style="background-color:#3498db; color:white; padding:15px; border-radius:5px;",
                      h5("Total Confirmed"), h4(sum(data$Confirmed)))),
        column(3, div(style="background-color:#e74c3c; color:white; padding:15px; border-radius:5px;",
                      h5("Total Deaths"), h4(sum(data$Deaths)))),
        column(3, div(style="background-color:#2ecc71; color:white; padding:15px; border-radius:5px;",
                      h5("Total Recovered"), h4(sum(data$Recovered)))),
        column(3, div(style="background-color:#f39c12; color:white; padding:15px; border-radius:5px;",
                      h5("Mortality Rate (%)"), h4(round(max(data$MortalityRate),2))))
      )
    )
  })
  
  # Plots

  output$confirmedPlot <- renderPlotly({
    plot_ly(filtered_data(), x=~Date, y=~Confirmed, type='scatter', mode='lines+markers', line=list(color='blue')) %>%
      layout(title=paste("Confirmed Cases in", input$country), xaxis=list(title="Date"), yaxis=list(title="Cases"))
  })
  
  output$deathsPlot <- renderPlotly({
    plot_ly(filtered_data(), x=~Date, y=~Deaths, type='scatter', mode='lines+markers', line=list(color='red')) %>%
      layout(title=paste("Deaths in", input$country), xaxis=list(title="Date"), yaxis=list(title="Deaths"))
  })
  
  output$recoveredPlot <- renderPlotly({
    plot_ly(filtered_data(), x=~Date, y=~Recovered, type='scatter', mode='lines+markers', line=list(color='green')) %>%
      layout(title=paste("Recovered Cases in", input$country), xaxis=list(title="Date"), yaxis=list(title="Recovered"))
  })
  
  # Map
  map_data <- reactive({
    latest_date <- max(filtered_data()$Date)
    covid_data %>%
      filter(Country.Region == input$country, Date == latest_date) %>%
      group_by(Province.State, Lat, Long) %>%
      summarise(Confirmed=sum(Confirmed), Deaths=sum(Deaths), Recovered=sum(Recovered), .groups="drop")
  })
  
  output$covidMap <- renderLeaflet({
    data <- map_data()
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Long, lat = ~Lat,
        radius = ~sqrt(Confirmed)/5,
        color = "blue",
        stroke = FALSE, fillOpacity = 0.5,
        popup = ~paste0("<strong>", Province.State, "</strong><br>",
                        "Confirmed: ", Confirmed, "<br>",
                        "Deaths: ", Deaths, "<br>",
                        "Recovered: ", Recovered)
      ) %>%
      setView(lng = mean(data$Long, na.rm=TRUE),
              lat = mean(data$Lat, na.rm=TRUE),
              zoom = 4)
  })
}
shinyApp(ui=ui, server=server)

