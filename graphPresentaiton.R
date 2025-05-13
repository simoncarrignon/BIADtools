require(sf)
devtools::load_all("../BIADconnect/")
require(rnaturalearth)
require(rnaturalearthdata)

conn <- init.conn()
allsites  <-  query.database(conn = conn,sql.command = "SELECT * FROM Sites")
current_date <- format(Sys.Date(), "%Y%m%d")
file_name <- paste0("allsites_", current_date, ".RDS")
saveRDS(file=file_name,allsites)
allsites=readRDS(file=file_name)
world <- ne_countries(scale = "medium", returnclass = "sf")

allsite_sf <- st_as_sf(allsites, coords = c("Longitude", "Latitude"), crs =st_crs(world))
area_of_interest  <- c( "France","Germany","Netherlands","Sweden", "Great Britain","Belgium","Luxembourg","Switzerland", "Ireland","Denmark","Czechia / Czech Republic","Poland", "Austria" ,"Andorra","Isle of Man", "Slovakia","Liechtenstein","Serbia","Croatia", "Hungary","Romania","Bosnia and Herzegovina","Bulgaria", "Latvia","North Macedonia","Ukraine","Slovenia", "Montenegro", "Guernsey","Russia","Estonia","Lithuania", "Albania","Kosovo","Moldova", "Norway","Finland", "Belarus")
allsites_lim = allsites[ allsites$Country %in%  area_of_interest,]
allsite_sf <- st_as_sf(allsites_lim, coords = c("Longitude", "Latitude"), crs =st_crs(world))
## Get countries that contains the world (not enough as this include countries spreading overseas)
sel_countries  <-  st_intersects(world,allsite_sf)
wd  <-  world[lengths(sel_countries)>0,]
wdsimpl <- st_simplify(world,10000)

#Simplify geography of the world
wd <- st_geometry(wd)
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
st_write(grid_lim,dsn="grid_europe.gpkg")
st_write(grid_lim,dsn="grid_europe_lim.gpkg")
plot(st_sf(grid_lim,len=log(lengths(inter)))[,"len"],main="(log) #Site per Hxagons",reset=F,pal=viridis::turbo)
plot(wd,add=T,lwd=2)
saveRDS(file="grid_europe.RDS",grid)
saveRDS(file="grid_europe_lim.RDS",grid_lim)


plot(st_sf(grid_lim,len=log(lengths(inter)))[,"len"],main="(log) #Site per Hxagons",reset=F,pal=viridis::turbo)
plot(wd,add=T,lwd=2)

png("aoi_points.png",width=1500,height=1300,pointsize=22)
par(bg=adjustcolor("light blue",.2),mar=c(1,1,1,1))
plot(st_crop(st_geometry(allsite_sf),st_bbox(aoi)),pch=20,col=NA)
plot(wdsimpl,add=T,lwd=2,col="white")
plot(st_crop(st_geometry(allsite_sf),st_bbox(aoi)),pch=21,bg=adjustcolor("red",.2),cex=1.2,lwd=.2,add=T)
dev.off()

png("aoi_hexagon.png",width=1600,height=1300,pointsize=22)
par(bg=adjustcolor("light blue",.2),mar=c(1,1,1,1))
plot(st_sf(grid_lim,len=lengths(inter))[,"len"],main="#Site per Hexagons",reset=F,pal=viridis::magma,logz=T)
plot(wdsimpl,add=T,lwd=2,col="white")
plot(st_sf(grid_lim,len=lengths(inter))[,"len"],main="#Site per Hexagons",pal=viridis::magma,add=T,logz=T)
plot(st_geometry(wdsimpl),add=T,lwd=2)
dev.off()

 alldata <- readRDS(paste0("tableTaxaPerMetasites.RDS"))

png("aoi_num_tax_hexagon.png",width=1600,height=1300,pointsize=22)
par(bg=adjustcolor("light blue",.2),mar=c(1,1,1,1))
plot(st_sf(grid_lim,len=apply(alldata,1,function(i)sum(i>0))),main=" #num taxa per Hex",pal=viridis::cividis,reset=F,logz=T)
plot(wdsimpl,add=T,lwd=2,col="white")
plot(st_sf(grid_lim,len=apply(alldata,1,function(i)sum(i>0))),main=" #num taxa per Hex",pal=viridis::cividis,add=T,logz=T)
plot(st_geometry(wdsimpl),add=T,lwd=2)
dev.off()

png("aoi_nisp_hexagon.png",width=1600,height=1300,pointsize=22)
par(bg=adjustcolor("light blue",.2),mar=c(1,1,1,1))
plot(st_bind_cols(grid_lim,len=apply(alldata,1,sum))[,"len"],main="total #nisp per Hex",pal=viridis::cividis,reset=F,logz=T)
plot(wdsimpl,add=T,lwd=2,col="white")
plot(st_bind_cols(grid_lim,len=apply(alldata,1,sum))[,"len"],main="total #nisp per Hex",pal=viridis::cividis,add=T,logz=T)
plot(st_geometry(wdsimpl),add=T,lwd=2)
dev.off()

alltopten  <- apply(alldata,1,function(line)sort(line,decreasing=T)[1:10],simplify=F)
topten <- unlist(lapply(alltopten[sapply(alltopten,sum)>0],names))

png("counts.png",width=1600,height=1300,pointsize=20)
par(mfrow=c(2,1))
counts <- apply(alldata,2,sum)
barplot(sort(counts,decreasing = T)[1:50],las=3,cex.names=1,border=0,col=adjustcolor("chartreuse",.7),log="y",ylab="NISP")
text(20,500000,label="NISP per taxon",cex=1.5,font=2,pos=4)
counts <- table(topten)
barplot(sort(counts,decreasing = T)[1:50],las=3,cex.names=1,border=0,col=adjustcolor("orange",.7),log="y",ylab="# hexagon")
text(20,100,label="Number of time in top10",cex=1.5,font=2,pos=4)
dev.off()


groups <- paste0("group_",as.roman(1:9))
names(groups) <- paste0("group ",as.roman(1:9))

allgroups <- lapply(groups,function(i){
    gfile  <- paste0(i,".csv")
    gdata <- read.csv(gfile,skip=1,header=F)
    gdata  <- gdata[!is.na(gdata[,1]),]
    txl=trimws(gsub("=","",gdata[,2]))
    currname <- paste(group,paste0(txl,collapse=" vs "))
    colnames(gdata)=c("","group","list of taxons")
    lot  <-  apply(gdata,1,function(gpe)list(group=gpe[2],lot=split_plus(gpe[3])))
    grouped <- sapply(lot,function(group){
                      taxons = group$lot[group$lot %in% colnames(alldata)]
                      apply(alldata[,taxons,drop=F],1,sum)
    })
    colnames(grouped)=txl
    return(grouped)
})


png("groups.png",width=1600,height=1600,pointsize=32)
par(mfrow=c(3,3),mar=c(1,1,3,1),oma=c(1,8,1,4),xpd=NA)
na <- lapply(names(allgroups),function(gn)pie(apply(allgroups[[gn]],2,sum),col=palette.colors(n=8,pal="Pastel 2"),main=gn,xpd=NA))
dev.off()

lapply(names(allgroups),function(gn){
    grouped <- allgroups[[gn]]
    currname <- paste0(colnames(grouped),collapse=", ")
    empty <- which(apply(grouped, 1, sum) <100 )
    caGroupeF <- CA(grouped[-empty, ],graph=F)
    caval <- NULL
    if(is.null(dim(caGroupeF$row$coord))){
        caval1 <- caGroupeF$row$coord
        png(paste0("CA_dim1_",groups[[gn]],".png"),width=1600,height=1600,pointsize=32)
        par(bg=adjustcolor("light blue",.2),oma=c(1,1,2,1))
        plot(st_sf(grid_lim[-empty, ], caval1), reset = FALSE, main = NULL,pal=viridis::viridis)
        plot(wdsimpl,add=T,lwd=2,col="white")
        plot(st_sf(grid_lim[-empty, ], caval1), add = TRUE, main = NULL,pal=viridis::viridis)
        plot(st_geometry(wdsimpl),add=T,lwd=2)
        mtext( paste0(gn," - dim 1 CA count for:\n", currname),3,0,cex=.8,font=2,outer=T)
        dev.off()
    }
    else {
        caval1 <- caGroupeF$row$coord[,1]
        caval2 <- caGroupeF$row$coord[,2]
        png(paste0("CA_dim1_",groups[[gn]],".png"),width=1000,height=1000,pointsize=32)
        par(bg=adjustcolor("light blue",.2),oma=c(1,1,2,1))
        plot(st_sf(grid_lim[-empty, ], caval1), reset = FALSE, main = NULL,pal=viridis::viridis,key.length=.5,cex.axis=.7)

        plot(wdsimpl,add=T,lwd=2,col="white")
        plot(st_sf(grid_lim[-empty, ], caval1), add = TRUE, main = NULL,pal=viridis::viridis)
        plot(st_geometry(wdsimpl),add=T,lwd=2)
        mtext( paste0(gn," - dim 1 CA count for:\n", currname),3,0,cex=.8,font=2,outer=T)
        dev.off()
        png(paste0("CA_dim2_",groups[[gn]],".png"),width=1000,height=1000,pointsize=32)
        par(bg=adjustcolor("light blue",.2),oma=c(1,1,2,1))
        plot(st_sf(grid_lim[-empty, ], caval2), reset = FALSE, main = NULL,pal=viridis::viridis,key.length=.5,cex.axis=.7)

        plot(wdsimpl,add=T,lwd=2,col="white")
        plot(st_sf(grid_lim[-empty, ], caval2), add = TRUE, main = NULL,pal=viridis::viridis)
        plot(st_geometry(wdsimpl),add=T,lwd=2)
        mtext( paste0(gn," - dim 2 CA count for:\n", currname),3,0,cex=.8,font=2,outer=T)
        dev.off()
        png(paste0("CA_",groups[[gn]],".png"),width=1000,height=1000,pointsize=32)
        print(plot.CA(caGroupeF,cex=2,cex.axis=2,cex.legend=2,cex.title=2,cex.main=2,cex.label=2))
        dev.off()
    }

    percentage <- grouped[-empty,]
    tot <- apply(percentage,1,sum)
    percentage <- apply(percentage,2,function(i)i/tot)

    pca.perc <- FactoMineR::PCA(percentage,graph=F)
    png(paste0("PCA_dim1_",groups[[gn]],".png"),width=1000,height=1000,pointsize=32)
    par(bg=adjustcolor("light blue",.2),oma=c(1,1,2,1))
    plot(st_sf(grid_lim[-empty, ], pca.perc$ind$coord[,1]), reset = FALSE, main = NULL,pal=viridis::viridis,key.length=.5,cex.axis=.7)
    plot(wdsimpl,add=T,lwd=2,col="white")
    plot(st_sf(grid_lim[-empty, ], pca.perc$ind$coord[,1]), add = TRUE, main = NULL,pal=viridis::viridis)
    plot(st_geometry(wdsimpl),add=T,lwd=2)
    mtext( paste0(gn," - dim 1 PCA % for:\n", currname),3,0,cex=.8,font=2,outer=T)
    dev.off()
    if(ncol(pca.perc$ind$coord)>1){
        png(paste0("PCA_dim2_",groups[[gn]],".png"),width=1000,height=1000,pointsize=32)
        par(bg=adjustcolor("light blue",.2),oma=c(1,1,2,1))
        plot(st_sf(grid_lim[-empty, ], pca.perc$ind$coord[,2]), reset = FALSE, main = NULL,pal=viridis::viridis,key.length=.5,cex.axis=.7)
        plot(wdsimpl,add=T,lwd=2,col="white")
        plot(st_sf(grid_lim[-empty, ], pca.perc$ind$coord[,2]), add = TRUE, main = NULL,pal=viridis::viridis)
        plot(st_geometry(wdsimpl),add=T,lwd=2)
        mtext( paste0(gn," - dim 2 PCA % for:\n", currname),3,0,cex=.8,font=2,outer=T)
        dev.off()
    }
    png(paste0("PCA_",groups[[gn]],".png"),width=1000,height=1000,pointsize=32)
    print(plot.PCA(pca.perc,choix="var",cex=2,cex.axis=2,cex.legend=2, habillage = 'contrib',cex.title=2,cex.main=2,cex.label=2))
    dev.off()
})

for( subperiod in periods$Period){
    alldata <- readRDS(paste0(subperiod,"_tableTaxaPerMetasites.RDS"))
    togethr[[subperiod]]=list()
    print(subperiod)
    cat(paste0("\n\r\n# ", subperiod,"\n\r\n"))
    i=5
    group  <- paste0("group_",as.roman(i))
    gfile  <- paste0(group,".csv")
    gdata <- read.csv(gfile,skip=1,header=F)
    gdata  <- gdata[!is.na(gdata[,1]),]
    txl=trimws(gsub("=","",gdata[,2]))
    currname <- paste0(txl,collapse=", ")
    cat(paste0("\n\r\n## ", currname,"\n\r\n"))
    colnames(gdata)=c("","group","list of taxons")
    lot  <-  apply(gdata,1,function(gpe)list(group=gpe[2],lot=split_plus(gpe[3])))
    grouped <- sapply(lot,function(group){
                      taxons = group$lot[group$lot %in% colnames(alldata)]
                      apply(alldata[,taxons,drop=F],1,sum)
    })
    colnames(grouped)=txl
    togethr[[subperiod]]=apply(grouped,2,sum)

}

png("bytime.png",width=1600,height=1600,pointsize=32)
spt=do.call("rbind",togethr)
par(mar=c(8,4,2,2))
a=barplot(apply(spt,1,function(i)i/sum(i)),las=3,cex.names=1,border=0,col=palette.colors(n=8,alpha=.8,pal="Pastel 2"),ylab="% NISP",legend=T,args.legend=list(bg="white",x="bottomright"))
mtext(paste0("N = ",prettyNum(apply(apply(spt,1,function(i)i),2,sum),big.mark=",")," nisp"),at=a,3,1)
dev.off()
