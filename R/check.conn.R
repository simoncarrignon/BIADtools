check.conn <- function(conn = NULL, db.credentials=NULL){
    if(is.null(conn) || !tryCatch(DBI::dbIsValid(conn),error=function(err)FALSE) ){ #check if no connector has been provided, or if the connector doesnt work
        #print("no connector provided, creating one here connecting ")
        if(exists("conn", envir = .GlobalEnv))conn <- get("conn", envir = .GlobalEnv) #check if a connector already exist at global level
        if(is.null(conn) || !tryCatch(DBI::dbIsValid(conn),error=function(err)FALSE) ){
            #print("the global connector is not good, delete and retry ")
            disco <- disconnect()
            conn <- init.conn(db.credentials=db.credentials)
            assign("conn",conn,envir = .GlobalEnv)
        }
    }
}

