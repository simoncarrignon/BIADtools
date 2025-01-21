require(sf)
source("../BIADwiki/R/functions.R")
source("../BIADwiki/R/functions.database.connect.R")
require(rnaturalearth)
require(rnaturalearthdata)

conn  <-  init.conn()
#allsites  <-  query.database(conn = conn,sql.command = "SELECT * FROM SITES")
#saveRDS(file="allsites_210212025.RDS",allsites)
allsites=readRDS(file="allsites_210212025.RDS")
world <- ne_countries(scale = "medium", returnclass = "sf")

allsite_sf <- st_as_sf(allsites, coords = c("Longitude", "Latitude"), crs =st_crs(world))
area_of_interest  <- c( "France","Germany","Netherlands","Sweden", "Great Britain","Belgium","Luxembourg","Switzerland", "Ireland","Denmark","Czechia / Czech Republic","Poland", "Austria" ,"Andorra","Isle of Man", "Slovakia","Liechtenstein","Serbia","Croatia", "Hungary","Romania","Bosnia and Herzegovina","Bulgaria", "Latvia","North Macedonia","Ukraine","Slovenia", "Montenegro", "Guernsey","Russia","Estonia","Lithuania", "Albania","Kosovo","Moldova", "Norway","Finland", "Belarus")
allsites_lim = allsites[ allsites$Country %in%  area_of_interest,]
allsite_sf <- st_as_sf(allsites_lim, coords = c("Longitude", "Latitude"), crs =st_crs(world))
## Get countries that contains the world (not enough as this include countries spreading overseas)
sel_countries  <-  st_intersects(world,allsite_sf)
world  <-  world[lengths(sel_countries)>0,]

#Simplify geography of the world
wd <- st_geometry(world)
wd <- st_simplify(wd,10000)

plot(wd)

#not enough as said before, country overseas, needs manually crop
aoi <- st_crop(wd,st_bbox(c(xmin=-10,xmax=50,ymin=36,ymax=70)))
aoi <- st_union(aoi)
plot(aoi)

#create a grid
grid <- st_make_grid(aoi, cellsize = 1.5, square=FALSE)
#keep only hexagon that are  _on_ the grid
grid <- st_intersection(grid,aoi)

inter  <-  st_intersects(st_make_valid(grid),allsite_sf)
grid_lim <- grid[lengths(inter)>0] 
inter  <-  inter[lengths(inter)>0]

##simple plot to show the number of sites per grid
plot(st_bind_cols(grid_lim,len=log(lengths(inter)))[,"len"],reset=F)
st_write(grid,dsn="grid_europe.gpkg")
saveRDS(file="grid_europe.RDS",grid)

