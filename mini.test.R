source("../../BIADwiki/R/functions.R")
source("../../BIADwiki/R/functions.database.connect.R")

conn <- init.conn()



allcultures <- sapply(1:3,function(i){
query  <-  paste0("SELECT Culture",i," FROM Phases")
query.database(query,conn)
})
sort(table(unlist(allcultures)))
sort(table(unlist(allcultures)))
