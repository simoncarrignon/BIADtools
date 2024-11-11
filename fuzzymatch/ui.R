library(shiny)
library(leaflet)
library(shinyTree)

shinyUI(fluidPage(
  titlePanel("Let's dig BIAD a bit..."),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("table", "In which table is the element you are looking for?", choices = get_table_list(conn)),
      uiOutput("fields_ui"),
      textInput("location", "Enter a string to match:", ""),
      actionButton("find_matches", "Find Matches"),
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


