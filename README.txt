# COVID-19 Interactive Dashboard with Forecasting and Map Visualization

This R Shiny application provides an interactive dashboard for exploring global COVID-19 data, including confirmed cases, deaths, recoveries, and forecasting future case trends. It features interactive plots, a dynamic map, data download capability, and a dark mode theme for enhanced usability.

---
  
  ## Features
  
  - **Country Selection:** Choose any country to explore its COVID-19 data.
- **Date Range Filtering:** Filter data by a custom date range.
- **Interactive Plots:** View confirmed cases, deaths, and recovered cases as interactive Plotly charts.
- **Forecasting:** 30-day forecast of confirmed cases using Facebook's Prophet model.
- **Map Visualization:** Leaflet map showing case distribution by province/state for the selected country and date.
- **Data Download:** Export the filtered dataset as a CSV file.
- **Dark Mode:** Toggle between light and dark themes for better visual comfort.

---

## Data Source

Data is sourced from the [Johns Hopkins University CSSE COVID-19 dataset](https://github.com/CSSEGISandData/COVID-19), including:

- Confirmed cases  
- Deaths  
- Recovered cases  

The CSV files are stored locally and loaded at app startup.

---

## Installation and Setup

### Prerequisites

- R (version 4.0 or higher recommended)  
- RStudio (optional but recommended)

### Required R Packages

Install required packages if not already installed:

```r
install.packages(c("shiny", "tidyverse", "plotly", "leaflet", "lubridate", "prophet", "shinyWidgets", "bslib"))


