library(shiny)
library(geosphere) # For calculating distances
library(leaflet)
library(ggmap)
library(sf)
library(shinyTree)
library(data.tree)
source("functions.R")
source("functions.database.connect.R")
library(shinybusy)

add_busy_bar(color = "#FF0000",timeout=1000)

allsites=readRDS("sites_table.RDS")

# Register your Google Maps API key
register_google(key = "AIzaSyCAXld-xHp2fE6QfAn94vtojVgERyNjuW8") #This API key won't work anymore

conn <- init.conn() #if no parameters, you need our credential as environment variable, to put in .Renviron.

# Function to get the list of tables from the database
get_table_list <- function(conn) {
    alltables <- DBI::dbListTables(conn)
    alltables[!grepl("z.*_.*",alltables)]
}

# Retrieve field names for a specified table
get_field_list <- function(conn, table) {
    DBI::dbListFields(conn, table)
}
