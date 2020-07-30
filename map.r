library(rgdal)
library(rcartocolor)
library(raster)
library(BAMMtools)

dirD <- "/Users/hkropp/Google Drive/GIS/WA_dem/WA_dem"

dem <- raster(paste0(dirD,"/psdem/psdem_2005.asc"))

plot(dem)
crsInfo <- "+proj=utm +zone=10 +datum=NAD27 +x_0=500000 +lon_0 =-123 +units=m +no_defs +ellps=clrk66 +towgs84=0,0,0 "
crs(dem) <- crsInfo
range(na.omit(getValues(dem)))
quantile(na.omit(getValues(dem)), prob=seq(0,1, by=0.05))
#generate terrain 
slope <- terrain(dem, opt="slope")
asp <- terrain(dem, opt="aspect")
#create hillshade
hills <- hillShade(slope,asp)

#adjust raster ----
reclass <- matrix(c(-900,-1,0,
                    0,13000,1), byrow= TRUE)
landMask <- reclassify(dem,reclass)
hillL <- mask(hills,landMask,maskvalue = 0)

extCr <- c()

#mapping ----
colWater <- rev(carto_pal(6,"Teal"))
colLand7 <- c(colWater[6],carto_pal(8, "Antique"))
display_carto_pal(8,"ArmyRose")
colLand <- terrain.colors(9)
colLand2 <- c(colWater[6],"#698474","#436e4f","#669b7c","#b0a160","#935900","#594a4e","#e5d8bf","white")
colLand3 <- c(colWater[6],"#1a3c40","#144d53","#307672","#b7e1b5","#de774e","#684656","#1c1124","white")

colLand4 <- c(colWater[6],#water transition
              "#b8b2a6",#lowland wetland
              "#5c8d89",#forest
              "#74b49b",#higher elevation
              "#a7d7c5",#vegetation to treeline
              "#d7c79e",#high elev 1
              "#e08f62",#high elev 2
              "#a35638",#high elev 3
              "#dddddd")#mountain top
colLand5 <- c(colWater[6],#water transition
              "#6fb98f",#lowland wetland
              "#2c7873",#forest
              "#004445",#higher elevation
              "#004445",#vegetation to treeline
              "#ba7967",#high elev 1
              "#ffdecf",#high elev 2
              "#faeee7",#high elev 3
              "#dddddd")#mountain top


colLand6 <- c(colWater[6],#water transition
              "#b8b2a6",#lowland wetland
              "#616f39",#forest
              "#004a2f",#higher elevation
              "#004a2f",#vegetation to treeline
              "#004a2f",#high elev 1
              "#d8aeae",#high elev 2
              "#9791a0",#high elev 3
              "#dddddd")#mountain top

plot(seq(1,9), pch=19, col=colLand6)

watBreaks <- c(-1000,-600,-400,-200,-100,-50,-1)
landBreaks <-c(0,100,500,1200,1800,3800,4500,5500,13000) 
BreaksAll <- c(watBreaks,landBreaks)
cols <- c(colWater, colLand2)
cols2 <- c(colWater, colLand3)
cols3 <- c(colWater, colLand4)
cols4 <- c(colWater, colLand5)
cols5 <- c(colWater, colLand6)
cols6 <- c(colWater, colLand7)

plot(dem, breaks=BreaksAll,col=cols, legend=FALSE )
plot(hillL, col=grey(0:100/100), alpha=0.35,add=TRUE, legend=FALSE)

plot(dem, breaks=BreaksAll,col=cols2, legend=FALSE )
plot(hillL, col=grey(0:100/100), alpha=0.35,add=TRUE, legend=FALSE)

plot(dem, breaks=BreaksAll,col=cols3, legend=FALSE )
plot(hillL, col=grey(0:100/100), alpha=0.35,add=TRUE, legend=FALSE)

plot(dem, breaks=BreaksAll,col=cols4, legend=FALSE )
plot(hillL, col=grey(0:100/100), alpha=0.15,add=TRUE, legend=FALSE)

plot(dem, breaks=BreaksAll,col=cols5, legend=FALSE )
plot(hillL, col=grey(0:100/100), alpha=0.45,add=TRUE, legend=FALSE)

plot(dem, breaks=BreaksAll,col=cols6, legend=FALSE )
plot(hillL, col=grey(0:100/100), alpha=0.45,add=TRUE, legend=FALSE)

#read in shapefiles ----
hydro <- readOGR("/Users/hkropp/Google Drive/GIS/vector/water/DNR_Hydrography_-_Water_Bodies-shp/DNR_Hydrography_-_Water_Bodies.shp")

hydro@proj4string
hyrop <- spTransform(hydro, dem@crs)



plot(dem, breaks=BreaksAll,col=cols5, legend=FALSE )
plot(hillL, col=grey(0:100/100), alpha=0.45,add=TRUE, legend=FALSE)

plot(hyrop, add = TRUE, col="black")
