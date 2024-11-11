library(shiny)
library(leaflet)
library(shinyTree)

shinyUI(fluidPage(
  titlePanel("Distance based exploration"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Enter Name (optional):", ""),
      numericInput("latitude", "Latitude:", value = NULL, step = 0.0001),
      numericInput("longitude", "Longitude:", value = NULL, step = 0.0001),
      actionButton("find_matches", "Find Matches"),
      hr(),
      sliderInput("distance", "Distance (D) in km:", min = 1, max = 100, value = 10, step = 1),
      hr(),
      h4("Sites Found"),
      uiOutput("key_buttons"), # Output for clickable primary key buttons
      shinyTree("siteTree"),
      "Further (if any) information below"
    ),
    
    mainPanel(
      leafletOutput("map")
    )
  ),
  fluidRow(
		   column(width=12,
				  br(),
				  DT::DTOutput("selTxt")
		   )
  )
))


