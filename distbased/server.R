
get_elements <- function(x, element) {
	newlist=list()
	for(elt in names(x)){
		if(elt == element) newlist=append(newlist,x[[elt]])
		else if(is.list(x[[elt]])) newlist=append(newlist,get_elements(x[[elt]],element) )
	}
	return(newlist)
}

renderSitesOnMap <- function(df,  key = NULL, center,dist){
    colors="blue"
    if(!is.null(key)) colors = ifelse(df[["SiteID"]] == key, 'red', 'blue') # Change color for selected key
    if(is.null(df$notes)) df$notes = ""

    renderLeaflet({
        leaflet(data = df) %>%
        addTiles() %>%
            addCircles(
              lng = center['longitude' ], lat = center['latitude'],
              radius = dist * 1000, weight = 1, color = "#FF0000"
            ) %>%
        addCircleMarkers(lng = center['longitude'], lat = center['latitude'], popup = "POI",color="red") %>% 
        addCircleMarkers(lng = as.numeric(df$Longitude), lat = as.numeric(df$Latitude), popup = paste(df$SiteID,":",df$SiteName,",",df$notes," last update:",df$time_last_update),color=colors)
    })
}
selected_table  <- "Sites"


shinyServer(function(input, output, session) {


  resultData <- reactiveVal(NULL)
  userCoords <- reactiveVal(NULL)  # Hold the user coordinates

  # Logic triggered by Find Matches button
  observeEvent(input$find_matches, {
    output$siteTree <- renderTree(NULL)
    output$key_buttons <- renderUI(NULL)
               print(input$name)
               print(input$latitude)
               print(input$longitude)
    if ((is.na(input$name) || input$name == "") && (is.na(input$latitude) || is.na(input$longitude))) {
      showModal(modalDialog(
        title = "Input Error",
        "Please enter either a name or both latitude and longitude.",
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
    } else{
    print((!is.null(input$latitude) && !is.null(input$longitude)))
        if(!is.na(input$name) && input$name != ""){
            print("fromm lonlat")
            focus  <- geocode(input$name)
            user_coords <- c(Longitude=focus$lon,Latitude=focus$lat)
        }
        else if (!is.null(input$latitude) && !is.null(input$longitude)) {
            print("fromm lonlat")
            user_coords <- c(Longitude=input$longitude,Latitude=input$latitude)
        }
        coords <- cbind(allsites$Longitude, allsites$Latitude)
        userCoords(user_coords)  # Update result data
        distances <- distm(x = coords, y = user_coords, fun = distHaversine)
print(sum(distances <= input$distance * 1000))
        result <- allsites[distances <= input$distance * 1000, ,drop=F] # Convert km to meters
        print(paste("d:",input$distance," nrow:",nrow(result),",",nrow(allsites)))
        output$map <- renderLeaflet({
            leaflet() %>%
                addTiles() %>%
                addCircleMarkers(lng = user_coords["Longitude"], lat = user_coords["Latitude"], popup = "POI", color = "green")
        })
        leafletProxy("map") %>%
            addCircles(
                       lng = user_coords["Longitude"], lat = user_coords["Latitude"],
                       radius = input$distance * 1000, weight = 1, color = "#FF0000"
            ) 
        if(nrow(result)>0){
          # Iterate over each primary key
         output$key_buttons <- renderUI({
           lapply(result[, "SiteID"], function(key) {
              actionButton(inputId = paste0("key_", key), label = key)
           })
         })
        }
        resultData(result)  # Update result data

      
    }
  })

  observe({
    req(resultData(), input$distance)
    print("A")
    coords <- userCoords()
    result <- resultData()

    if(nrow(result)>0){
    # Update map using leafletProxy when distance changes
    leafletProxy("map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(
        lng = result$Longitude, lat = result$Latitude,
        popup = paste(result$SiteID, ":", result$SiteName, ",", result$notes, "last update:", result$time_last_update),
        color = "blue"
      )
      leafletProxy("map") %>%
         addCircles(
           lng = coords["Longitude"], lat = coords["Latitude"],
           radius = input$distance * 1000, weight = 1, color = "#FF0000"
         ) 
      print("newcoo")
      print(coords)
      leafletProxy("map") %>% addCircleMarkers(lng = coords["Longitude"], lat = coords["Latitude"], popup = "POI", color = "green")
      lapply(result[, "SiteID"], function(key) {
         observeEvent(input[[paste0("key_", key)]], {
             x <- run.searcher(table.name = "Sites", primary.value = key, conn = conn, direction = "down")$down
             print(x)
             
             get_json <- reactive({
                 treeToJSON(FromListSimple(x), pretty = TRUE)
             })
             output$siteTree <- renderTree(get_json())
             output$selTxt <- DT::renderDT({
                 tree <- input$siteTree
                 if (is.null(tree)){
                     NULL
                 } else{
                     alldatas=lapply(get_selected(tree),function(node){
                         if(node == 'data'){
                             uptree=attr(node, "ancestry")
                             data=x
                             for(i in 1:length(uptree)){
                                 data=data[[uptree[i]]]
                             }
                             return(data$data)
                         }
	     				else array(dim=c(0,0))
                     })
	     			if(length(alldatas)>0)return(alldatas[[1]])
	     			else NULL
                 }
             })
       })
      })
      }
	  else{
		  output$siteTree <- renderUI(NULL)
		  output$selTxt <- renderUI(NULL)
	  }
  })

})


