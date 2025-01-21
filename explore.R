library(sf)

source("../BIADwiki/R/functions.R")
source("../BIADwiki/R/functions.database.connect.R")
require(rnaturalearth)
require(rnaturalearthdata)


db.credentials <- list()
db.credentials$BIAD_DB_USER <- "simon carrignon"
db.credentials$BIAD_DB_PASS <- "simon carrignon"
db.credentials$BIAD_DB_HOST <- "macelab-server.biochem.ucl.ac.uk"
db.credentials$BIAD_DB_PORT <- 3306

conn  <-  init.conn(db.credentials)
allsites  <-  query.database(conn = conn,sql.command = "SELECT * FROM SITES")
world <- ne_countries(scale = "medium", returnclass = "sf")

allsite_sf <- st_as_sf(allsites, coords = c("Longitude", "Latitude"), crs =st_crs(world))

area_of_interest  <- c( "France","Germany","Netherlands","Sweden", "Great Britain","Belgium","Luxembourg","Switzerland", "Ireland","Denmark","Czechia / Czech Republic","Poland", "Austria" ,"Andorra","Isle of Man", "Slovakia","Liechtenstein","Serbia","Croatia", "Hungary","Romania","Bosnia and Herzegovina","Bulgaria", "Latvia","North Macedonia","Ukraine","Slovenia", "Montenegro", "Guernsey","Russia","Estonia","Lithuania", "Albania","Kosovo","Moldova", "Norway","Finland", "Belarus")

#allfaunal <- sapply(inter,function(sites) sapply(allsite_sf$SiteID[sites],get_all_elemets,conn=conn))
#allbotanical <- sapply(inter,function(sites) sapply(allsite_sf$SiteID[sites],get_all_elemets,conn=conn,elt="ABotSamples"))


alldata <- readRDS("tableTaxaPerMetasites.RDS")
grid=readRDS("grid_europe.RDS")

pca=prcomp(log(alldata+1))
norm=alldata/apply(alldata,1,sum)
plot(st_bind_cols(grid_lim,len=pca$x[,1]))
pca.data=FactoMineR::PCA(norm)
pca=FactoMineR::PCA(alldata)
plot(st_bind_cols(grid_lim,len=pca$ind$coord[,1]))
pca=FactoMineR::PCA(log(alldata+1))
plot(st_bind_cols(grid_lim,len=pca$ind$coord[,3]))

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
plot(st_bind_cols(grid_lim,total_nisp=log10(nnisp+1)),reset=F)
plot(wd,add=T,border="red",lwd=2)


nrows=sapply(grouped,nrow)
numtax <- unlist(ifelse(lengths(grouped)==0,0,nrows))
plot(st_bind_cols(grid_lim,len=log10(numtax)),reset=F)
#text(st_coordinates(st_centroid(st_make_valid(grid_lim))),labels=round(log10(numtax),digit=2))
plot(wd,add=T,border="red",lwd=2)


pcdata  <-  log(alldata[-c(which(nnisp<10)),]
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
