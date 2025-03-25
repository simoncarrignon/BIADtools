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

abot  <-  query.database(conn = conn,sql.command = "SELECT * from ABotSamples Join Phases On ABotSamples.PhaseID = Phases.PhaseID JOIN Sites ON Phases.SiteID = Sites.SiteID")

world <- ne_countries(scale = "medium", returnclass = "sf")

area_of_interest  <- c( "France","Germany","Netherlands","Belgium","Luxembourg","Switzerland", "Denmark")
allsites=abot
allsites_lim = allsites[ allsites$Country %in%  area_of_interest,]
allsite_sf <- st_as_sf(allsites_lim, coords = c("Longitude", "Latitude"), crs =st_crs(world))
world  <-  st_crop(world,st_bbox(c(xmin=-10,xmax=40,ymin=36,ymax=70)))
wd <- st_geometry(st_simplify(world,10000))
periods=read.csv("periods.csv")
grouping=strsplit(periods$PeriodID,"\\+")
names(grouping)= periods$Period
corresp = unlist(lapply(names(grouping),function(e){names(grouping[[e]])=grouping[[e]];grouping[[e]][]=e;grouping[[e]]}))

allsite_sf$general = NA
allsite_sf$general=corresp[allsite_sf$Period]

aoi <- st_crop(wd,st_bbox(c(xmin=-10,xmax=40,ymin=36,ymax=70))) 
par(mfrow=c(1,2))
for(period in c("Neolithic","Bronze Age")){
    plot(aoi,xlim=c(-9,20),ylim=c(42,59),main=period,bgc=adjustcolor("light blue",.2),col="white",lwd=.2)
    subset  <- allsite_sf [ allsite_sf$general == period,]
    size=table(subset$SiteID)
    plot(st_geometry(unique(subset[,c("SiteID")])),cex=log(size+1),add=T,pch=21,bg=adjustcolor("chartreuse",.3),lwd=.4)
    box()
    legend("topright",legend=c(100,10,1),title="Number of samples",pch=21,pt.cex=log(c(100,10,1)+1),pt.bg=adjustcolor("chartreuse",.4), y.intersp = 1.5, x.intersp = 1.8,bg="white")
}



allsites=abot
allsite_sf <- st_as_sf(allsites, coords = c("Longitude", "Latitude"), crs =st_crs(world))
sel_countries  <-  st_intersects(world,allsite_sf)
world  <-  world[lengths(sel_countries)>0,]
wd <- st_geometry(world)
wd <- st_simplify(wd,10000)
periods=read.csv("periods.csv")
grouping=strsplit(periods$PeriodID,"\\+")
names(grouping)= periods$Period
corresp = unlist(lapply(names(grouping),function(e){names(grouping[[e]])=grouping[[e]];grouping[[e]][]=e;grouping[[e]]}))
allsite_sf$general = NA
allsite_sf$general=corresp[allsite_sf$Period]
par(mfrow=c(1,3))
for(period in c("Neolithic","Eneolithic","Bronze Age")){
    plot(wd,main=period,bgc=adjustcolor("light blue",.2),col="white",lwd=.2)
    subset  <- allsite_sf [ allsite_sf$general == period,]
    size=table(subset$SiteID)
    plot(st_geometry(unique(subset[,c("SiteID")])),cex=log(size),add=T,pch=21,bg=adjustcolor("dark green",.3),lwd=.4)
    box()
}

