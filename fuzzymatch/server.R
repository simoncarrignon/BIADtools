
get_elements <- function(x, element) {
	newlist=list()
	for(elt in names(x)){
		if(elt == element) newlist=append(newlist,x[[elt]])
		else if(is.list(x[[elt]])) newlist=append(newlist,get_elements(x[[elt]],element) )
	}
	return(newlist)
}

renderSitesOnMap <- function(df,  key = NULL){
    colors="blue"
    if(!is.null(key)) colors = ifelse(df[["SiteID"]] == key, 'red', 'blue') # Change color for selected key
    if(is.null(df$notes)) df$notes = ""

    renderLeaflet({
        leaflet(data = df) %>%
        addTiles() %>%
        addCircleMarkers(lng = as.numeric(df$Longitude), lat = as.numeric(df$Latitude), popup = paste(df$SiteID,":",df$SiteName,",",df$notes," last update:",df$time_last_update),color=colors)
    })
}

shinyServer(function(input, output, session) {
  observe({
    tables <- get_table_list(conn)
    tables <- c("Sites", tables[tables != "Sites"])  # Start with "Sites" and append other tables
    updateSelectInput(session, "table", choices = tables)
  })

  observeEvent(input$table, {
    fields <- get_field_list(conn, input$table)
    if(input$table == "Sites") fields <- c("SiteName", fields[fields != "SiteName"])  
    output$fields_ui <- renderUI({
      selectInput("field", "Choose a field:", choices = fields)
    })
  })

  resultData <- reactiveVal(NULL)

  # Logic triggered by Find Matches button
  observeEvent(input$find_matches, {
    output$siteTree <- renderTree(NULL)
    output$key_buttons <- renderUI(NULL)
    output$map <- renderLeaflet(NULL)
    location <- input$location
    selected_table <- input$table
    selected_field <- input$field
    output$selTxt <- renderUI(NULL)
    
    if (nchar(location) > 0 && !is.null(selected_table) && !is.null(selected_field)) {
      # Construct a SQL query with the selected field and location
      query <- paste0("SELECT * FROM ",selected_table," WHERE ",selected_field," LIKE '%",location,"%'")
      print(query)
      result <- query.database(query,conn)
      
      # Store result in reactive variable
      if (!is.null(result) && nrow(result) > 0) {
         resultData(result)  # Update reactive value
         primaryKey <- get.primary.column.from.table(table.name = selected_table)
         mainKey <<- primaryKey

         # Generate UI for each primary key in the result
         output$key_buttons <- renderUI({
           lapply(result[, primaryKey], function(key) {
              actionButton(inputId = paste0("key_", key), label = key)
           })
         })
        result <- resultData()
        if (!is.null(result) && nrow(result) > 0 ) {
            if(selected_table == "Sites"){
                output$map <- renderSitesOnMap(result)
            }
            else{
                sites <- sapply(result[,primaryKey],function(key)get_elements(run.searcher(table.name=selected_table,primary.value=key,conn=conn,direction="up"),"Sites"))

				sites <- t(sapply(sites,function(i)i[,c("SiteID","SiteName","Latitude","Longitude")]))
				sites <- cbind.data.frame(sites, notes=paste0(primaryKey,": ",result[,primaryKey],","))
				print(sites)
                output$map <- renderSitesOnMap(sites)
			}
        } else {
          # Optionally provide feedback if there are no results to show
          output$selTxt <- renderPrint({ "No results to show on map" })
        }
      } else {
        resultData(NULL)  # Clear reactive value
        output$selTxt <- renderPrint("No results found")
      }
    }
  })


  observe({
      req(resultData())
      primaryKey <- mainKey
      result <- resultData()
      if (!is.null(result) && nrow(result) > 0) {
          # Iterate over each primary key
          lapply(result[, primaryKey], function(key) {
             observeEvent(input[[paste0("key_", key)]], {
                x <- run.searcher(table.name = input$table, primary.value = key, conn = conn, direction = "down")$down
                if(input$table == "Sites"){
                    output$map <- renderSitesOnMap(result,key)
                }
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


