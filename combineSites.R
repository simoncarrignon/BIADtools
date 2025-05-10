
#' Extract All Elements from a Site
#'
#' This function extracts all elements of a specified type from a given site. 
#' It retrieves elements from the tree structure associated with the site, which is created using the \code{get.relatives} function.
#'
#' @param site A character string specifying the site from which to extract elements.
#' @param conn A database connection object used by the \code{get.relatives} function to access the needed data.
#' @param elt A character string specifying the type of elements to extract (default is "FaunalSpecies").
#' @param log A logical indicating whether to print log messages (default is \code{FALSE}).
#' 
#' @return A list of elements matching the specified type extracted from the site.
#'
#' @examples
#' \dontrun{
#' conn <- init.conn()
#' site <- "S20559"
#' all_elements <- get_all_elemets(site, conn, elt = "FaunalSpecies", log = TRUE)
#' }
get_all_elemets <- function(site,conn,elt="FaunalSpecies",log=FALSE){
    tree=get.relatives(table.name="Sites",primary.value=site,directions="down",conn=conn)
    if(log) print(paste0("extract", elt, "from",site))
    extracted=get_elements(tree,elt)
    return(unname(extracted))
}

allfaunal=list()
allbotanical=list()
for(i in 1:length(inter)){
    print(paste0("I",i))
    if(is.null(allfaunal[[paste0("I",i)]])){
        allres=list()
        for(site in allsite_sf$SiteID[inter[[i]]]){
            allres <- append(allres,get_all_elemets(site,conn=conn))
        }
        allfaunal[[paste0("I",i)]]=allres
    }
    #if(is.null(allbotanical[[paste0("I",i)]])) allbotanical[[paste0("I",i)]]=lapply(allsite_sf$SiteID[inter[[i]]],get_all_elemets,conn=conn,elt="ABotSamples")
    else print("alread done")
}
alltaxon=lapply(allfaunal,function(i)do.call("rbind",i))
grouped=lapply(alltaxon,function(data)if(all(c("TaxonCode", "NISP") %in% names(data)))aggregate(NISP ~ TaxonCode, data=data,sum,na.rm=TRUE))

alltaxonname <- sort(unique(unlist(sapply(grouped,function(i)if("TaxonCode" %in%  colnames(i))i[,"TaxonCode"]))))
alldata=matrix(ncol=length(alltaxonname),nrow=length(grouped))
alldata[,]=0
colnames(alldata)=alltaxonname

for(i in 1:length(grouped)){
    curr=grouped[[i]]
    tax=as.character(unlist(curr[,1]))
    print(unlist(curr[,2]))
    alldata[i,tax]=unlist(curr[,2])
}

saveRDS(file="tableTaxaPerMetasites.RDS",alldata)
