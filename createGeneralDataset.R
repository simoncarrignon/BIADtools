allfaunal <- readRDS("ListAllFaunalPerSitePerMetaSites.RDS")
allsites <- readRDS("allsites_210212025.RDS")
depptable <- as.data.frame(matrix(ncol=29,nrow=0))
for( reg in 1:length(allfaunal)){
    if(length(allfaunal[[reg]])>0){
        for( site in 1:length(allfaunal[[reg]])){
            print(dim(newline))
            print(paste0(reg,",",site))
            newline=cbind(SiteID=allsites$SiteID[inter[[reg]][site]],Region=paste0("I",reg))
            depptable <- rbind(depptable,newline)
        }
    }
}


Sorry I should have formulated differently ; the lost of the regions/hexagons  is mainly due to hexagon that have no site with no NISP  _at all_.
To clarrify: in the region we selected, we have 370 hexagons with at least 1 site within it. Then out of these 370 only 188 have NISP, and out of these 188 only 170 have NISP from the selected group (the the loss is more of 10-20 hexagons, which sound more plausible). I will still doublecheck and maybe create a general table that Jelena can check too!
Simon


