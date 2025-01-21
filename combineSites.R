#' Get Elements from a Tree Structure
#'
#' This function extracts from a tree, as created by the function \code{get.relatives}, 
#' all nodes/leaves that have names corresponding to the specified element.
#' trees should be as named list of list like list(a=1,b=list(a=2,b=3,c=4),d=list(e=1,f=4))
#'
#' @param x A list representing the tree structure.
#' @param element A character string specifying the name of elements to extract from the tree.
#' 
#' @return A list containing all elements from the tree that have names matching the specified element.
#'
#' @examples
#' \dontrun{
#' tree <- list(a = 1, b = list(a = 2, c = 3), d = 4)
#' get_elements(tree, "a")
#' # Expected output: list(1, 2)
#' }
get_elements <- function(x, element) {
	newlist=list()
	for(elt in names(x)){
		if(elt == element) newlist=append(newlist,x[[elt]])
		else if(is.list(x[[elt]])) newlist=append(newlist,get_elements(x[[elt]],element) )
	}
	return(newlist)
}

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

saveRDS(file"tableTaxaPerMetasites.RDS",alldata)
