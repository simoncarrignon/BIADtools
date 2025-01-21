library(sf)
get_elements <- function(x, element) {
	newlist=list()
	for(elt in names(x)){
		if(elt == element) newlist=append(newlist,x[[elt]])
		else if(is.list(x[[elt]])) newlist=append(newlist,get_elements(x[[elt]],element) )
	}
	return(newlist)
}

source("../BIADwiki/R/functions.R")
source("../BIADwiki/R/functions.database.connect.R")
require(rnaturalearth)
require(rnaturalearthdata)


conn  <-  init.conn()
allsites  <-  query.database(conn = conn,sql.command = "SELECT * FROM SITES")
world <- ne_countries(scale = "medium", returnclass = "sf")

 query.database(conn = conn,sql.command = "SELECT * FROM Phases where PhasesId == BCX")

allsite_sf <- st_as_sf(allsites, coords = c("Longitude", "Latitude"), crs =st_crs(world))
world  <- 


# Crop the world map to the extent of the bounding box
world_cropped <- st_crop(world, st_buffer(allsite_sf,100))
world_cropped <- st_make_grid(world, st_buffer(allsite_sf,100))

# Create a grid over the cropped area
grid_spacing <- 1  # Specify the spacing for the grid in degrees
grid <- st_make_grid(world_cropped, cellsize = grid_spacing, square = FALSE) # square = FALSE for hexagons

# Filter the grid to keep only the cells intersecting land
land_grid <- st_intersection(grid, world_cropped)
plot(st_geometry(world_cropped))
plot(st_geometry(world),add=T)
plot(st_geometry(allsite_sf),col="red")
plot(st_geometry(grid),add=TRUE)
area_of_interest  <- c( "France","Germany","Netherlands","Sweden", "Great Britain","Belgium","Luxembourg","Switzerland", "Ireland","Denmark","Czechia / Czech Republic","Poland", "Austria" ,"Andorra","Isle of Man", "Slovakia","Liechtenstein","Serbia","Croatia", "Hungary","Romania","Bosnia and Herzegovina","Bulgaria", "Latvia","North Macedonia","Ukraine","Slovenia", "Montenegro", "Guernsey","Russia","Estonia","Lithuania", "Albania","Kosovo","Moldova", "Norway","Finland", "Belarus")
allsites_lim = allsites[ allsites$Country %in%  area_of_interest,]
allsite_sf <- st_as_sf(allsites_lim, coords = c("Longitude", "Latitude"), crs =st_crs(world))
plot(st_geometry(allsite_sf),pch=20,col="red")
sel_countries  <-  st_intersects(world,allsite_sf)
world  <-  world[lengths(sel_countries)>0,]
wd <- st_geometry(world)
plot(wd)
wd <- st_simplify(wd,10000)
plot(wd)
plot( st_crop(x=wd, y=st_bbox(allsite_sf)))
world_cropped <- st_make_valid(world_cropped)
aoi <- st_crop(wd,st_bbox(c(xmin=-10,xmax=50,ymin=36,ymax=70)))
aoi <- st_union(aoi)
grid <- st_make_grid(aoi, cellsize = 1.5, square=FALSE)
grid <- st_intersection(grid,aoi)
plot(grid)
inter  <-  st_intersects(st_make_valid(grid),allsite_sf)
grid_lim=grid[lengths(inter)>0] 
inter  <-  inter[lengths(inter)>0]
plot(st_bind_cols(grid_lim,n_sites=log(lengths(inter)))[,"n_sites"],reset=F)
allfaunal <- sapply(inter,function(sites) sapply(allsite_sf$SiteID[sites],get_all_elemets,conn=conn))
allbotanical <- sapply(inter,function(sites) sapply(allsite_sf$SiteID[sites],get_all_elemets,conn=conn,elt="ABotSamples"))

#extract all C14, bones, Graves,kk
get_all_elemets <- function(site,conn,elt="FaunalSpecies"){
    tree=get.relatives(table.name="Sites",primary.value=site,directions="down",conn=conn)
    print(site)
    print(elt)
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


pca=prcomp(log(alldata+1))
norm=alldata/apply(alldata,1,sum)
plot(st_bind_cols(grid_lim,len=pca$x[,1]))
pca.data=FactoMineR::PCA(norm)
pca=FactoMineR::PCA(alldata)
plot(st_bind_cols(grid_lim,len=pca$ind$coord[,1]))
pca=FactoMineR::PCA(log(alldata+1))
plot(st_bind_cols(grid_lim,len=pca$ind$coord[,1]))

norm=alldata/apply(alldata,1,sum)
pca=FactoMineR::PCA(norm)
plot(st_bind_cols(grid_lim,len=pca$ind$coord[,1]))

norm=alldata/apply(alldata,1,sum)
pca=FactoMineR::PCA(log(norm+0.00001))
plot(st_bind_cols(grid_lim,len=pca$ind$coord[,1]))
plot(st_bind_cols(grid_lim,len=pca$ind$coord[,1]))
pca=FactoMineR::PCA(norm)
plot(st_bind_cols(grid_lim,len=pca$ind$coord[,1]))

presabs=alldata
presabs[presabs>0]=1
pca=FactoMineR::PCA(presabs)
plot(st_bind_cols(grid_lim,len=log(pca$ind$coord[,1]+10)))

plot(density(apply(alldata,2,sum)),log="x")
sapply(c(1,10,100),function(l) sum(apply(alldata,2,sum)<l))
limdat=alldata[,apply(alldata,2,sum)>10]

norm=limdat/apply(limdat,1,sum)
pca.data=FactoMineR::PCA(norm)
plot(st_bind_cols(grid_lim,len=pca.data$ind$coord[,1]))
pca=FactoMineR::PCA(limdat)
plot(st_bind_cols(grid_lim,len=pca$ind$coord[,1]))
pca=FactoMineR::PCA(log(limdat+1))
plot(st_bind_cols(grid_lim,len=pca$ind$coord[,1]))
plot(st_bind_cols(grid_lim,len=pca$ind$coord[,1]))
norm=limdat/apply(limdat,1,sum)
pca.data=FactoMineR::PCA(norm)

null=which(apply(limdat[-c(78,82),],1,sum)==0)
ca=FactoMineR::CA(log(limdat[-c(78,82),]+1))
plot(st_bind_cols(grid_lim[-c(null,78,82),],len=ca$row$coord[,1]))
norm=limdat[-c(78,82),]/apply(limdat[-c(78,82),],1,sum)
pca.data=FactoMineR::PCA(norm)
plot(st_bind_cols(grid_lim[-c(78,82),],len=pca.data$ind$coord[,1]))
pca=FactoMineR::PCA(limdat)
plot(st_bind_cols(grid_lim,len=pca$ind$coord[,1]))
pca=FactoMineR::PCA(log(limdat+1))
plot(st_bind_cols(grid_lim[-c(78,82)],len=pca.data$ind$coord[-c(78,82),1]))


#notes: set up meeting with ejelena to show maps of nmber of faunal remain pero 'hexagons':
# - map 1: allsites with remains
# - hexagones with counts
plot(st_geometry(grid_lim))
points(st_coordinates(allsite_sf),pch=21,bg="green",lwd=.1)
plot(wd,add=T,border="red",lwd=2)


nnisp=sapply(grouped,function(a)sum(a[,2]))
plot(st_bind_cols(grid_lim,total_nisp=log10(nnisp)),reset=F)
plot(wd,add=T,border="red",lwd=2)


nrows=sapply(grouped,nrow)
numtax <- unlist(ifelse(lengths(grouped)==0,0,nrows))
plot(st_bind_cols(grid_lim,len=log10(numtax)),reset=F)
#text(st_coordinates(st_centroid(st_make_valid(grid_lim))),labels=round(log10(numtax),digit=2))
plot(wd,add=T,border="red",lwd=2)


pcdata  <-  alldata[-c(which(nnisp<10)),]
pcsites <- allsites[-c(which(nnisp<10)),]
pcgrid <- grid_lim[-c(which(nnisp<10)),]

ca=FactoMineR::CA(pcdata)
plot(st_bind_cols(pcgrid,ca=ca$row$coord[,1]),reset=F)

pca=FactoMineR::PCA(pcdata)
plot(st_bind_cols(pcgrid,pca=pca$ind$coord[,1]),reset=F)
plot(wd,add=T,border="red",lwd=2)





## Create a map and CSV with all sites faunal isotopes and NISP

## Meeting 16/12/2024 with Jelen
## GMS // GMM , what are they? they seem wrong
Meeting with Adrian:
-how date are generate/added
Getting list of things to exxlude
