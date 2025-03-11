source("../BIADwiki/R/functions.R")
source("../BIADwiki/R/functions.database.connect.R")
library(sf)
conn  <-  init.conn()

allsites  <-  query.database(conn = conn,sql.command = "SELECT * FROM SITES")

world <- ne_countries(scale = "medium", returnclass = "sf")
allsite_sf <- st_as_sf(allsites[!(is.na(allgrave$AgeCategorical) & (is.na(allgrave$AgeMin) & is.na(allgrave$AgeMax))),], coords = c("Longitude", "Latitude"), crs =st_crs(world))
wd <- st_geometry(world[world$continent == "Europe" | world$name_en %in% c("Turkey","Armenia","Georgia","Azerbaijan","Kazakhstan"),])
wd <- st_simplify(wd,10000)
aoi <- st_crop(wd,st_bbox(c(xmin=-10,xmax=35,ymin=28,ymax=70)))
aoi <- st_union(aoi)
aoi_mod <- st_transform(aoi, crs = 25832)
plot(st_geometry(aoi_mod),col=adjustcolor("red",.1),lwd=.6)
aoi_sites <- allsite_sf[st_intersects(aoi,allsite_sf)[[1]],]
plot(st_geometry(st_transform(aoi_sites,crs=st_crs(aoi_mod))),add=T,pch=20,cex=.7)
