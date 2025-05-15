

#Get biad sites  and rivers
conn <- init.conn()
allsites  <-  query.database(conn = conn,sql.command = "SELECT * FROM Sites")
rivers  <- st_read("rivers_europe_37253.shp")

#merges things
river_outline <- st_concave_hull(st_union(st_geometry(rivers)),ratio=.1)
overlap <- st_intersection(allsite_sf,river_outline)
overlap_land<-st_intersection(world,river_outline)
overlap<-st_intersection(allsite_sf,overlap_land)

# plot mergin exercices
par(mfrow=c(1,1))
plot(st_geometry(st_union(overlap_land)),col=adjustcolor("green",.4))
plot(river_outline,add=T)
plot(st_geometry(rivers),add=T,lwd=.3)
plot(st_geometry(overlap),add=T,col="red",pch=20,cex=.4)

# randomly sample
randsamp=st_sample(st_union(overlap_land),nrow(overlap))
plot(st_geometry(randsamp),col="black",add=T,pch=21,bg=adjustcolor("blue",.6),cex=.4,lwd=.3)

# compute distance to all river, take the min
cl<-makeCluster(20,outfile="log.txt")
distsamp <- parLapply(cl,1:length(randsamp),function(i,overlap,rivers){cat(paste0(i,"/",length(overlap),"\r"));min(sf::st_distance(overlap[i,],rivers))},overlap=randsamp,rivers=rivers)
print("done")
distmin <- parLapply(cl,1:nrow(overlap),function(i,overlap,rivers){cat(paste0(i,"/",nrow(overlap),"\r"));min(sf::st_distance(overlap[i,],rivers))},overlap=overlap,rivers=rivers)
stopCluster(cl)

## cutoff at 10 meters 
alldistsamp=unlist(distsamp)
alldistsamp[alldistsamp<10]=10
alldistmin=unlist(distmin)
alldistmin[alldistmin<10]=10
saveRDS(file="distances.RDS",list(data=alldistmin,sample=alldistsamp))

# a few graph to show things
png("distrib.png",width=1200jj)
par(mfrow=c(1,3))
logdensdat=density(log10(alldistmin),bw=0.2,from=1,to=6)
logdenssamp=density(log10(alldistsamp),bw=0.2,from=1,to=6)
densdat=density(alldistmin,bw=20,from=10,to=20000)
denssamp=density(alldistsamp,bw=20,from=10,to=20000)
plot(1,1,type="n",xlim=range(c(densdat$x,denssamp$x)),ylim=range(c(densdat$y,denssamp$y)),xlab="distance to closest river (m)",main="density distribution")
lines(densdat,lwd=2,col="red")
lines(denssamp,lwd=2,col="blue",lty=2)
#legend("right", legend=c("data","random sample"),col=c("red","blue"),lty=1:2)
plot(1,1,type="n",xlim=range(c(logdensdat$x,logdenssamp$x),na.rm=T),ylim=range(c(logdensdat$y,logdenssamp$y),na.rm=T),xlab="log 10 distance to closest river (m)",main="log 10 density")
lines(logdensdat,lwd=2,col="red")
lines(logdenssamp,lwd=2,col="blue",lty=2)
legend("right", legend=c("data","random sample"),col=c("red","blue"),lty=1:2)
plot(ecdf(log10(alldistmin)),col="red",lwd=2,main="CDF",xlab="log 10 distance to map")
lines(ecdf(log10(alldistsamp)),col="blue",lwd=2,lty=2)
dev.off()

#twenty farthest sites)
par(mfrow=c(1,2))
plot(st_geometry(st_union(overlap_land)),col=adjustcolor("green",.4),main="outliers: highland and coasts")
plot(st_geometry(overlap[alldistmin>2*10^4,]),add=T,cex=2,col="orange",pch=20)
hist(alldistmin[alldistmin>2*10^4],main="distribution of outliers",xlab="distance to river (m)")

