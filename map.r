library(rgdal)
library(rcartocolor)
library(raster)
library(BAMMtools)
library(extrafont)

dirD <- "/Users/hkropp/Google Drive/GIS/WA_dem/WA_dem"
dirOut <- "/Users/hkropp/Google Drive/research/mapping/WA_dem"

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
#set up colors
colWater <- rev(carto_pal(6,"Teal"))


colLand5 <- c(colWater[6],#water transition
              "#6fb98f",#lowland wetland
              "#2c7873",#forest
              "#004445",#higher elevation
              "#004445",#vegetation to treeline
              "#ba7967",#high elev 1
              "#ffdecf",#high elev 2
              "#faeee7",#high elev 3
              "#dddddd")#mountain top


plot(seq(1,9), pch=19, col=colLand6)
#set up breaks
watBreaks <- c(-1000,-600,-400,-200,-100,-50,-1)
landBreaks <-c(0,100,500,1200,1800,3800,4500,5500,13000) 
BreaksAll <- c(watBreaks,landBreaks)

cols4 <- c(colWater, colLand5)

dem@extent
#change extent
extD <- c(1000000,1340000,10000,530000 )


#total height
font_add("Raleway", "Raleway-Regular.ttf")



png(paste0(dirOut, "/WA_dem.png"), height = 30, width = 20, units = "in", res=300)

par(family="Raleway",mai=c(2,1,2,1))
plot(dem, breaks=BreaksAll,col=cols4,ext = extD, legend=FALSE,axes=FALSE, bty="n" )

plot(hillL, col=grey(0:100/100), alpha=0.15, ext = extD, add=TRUE, legend=FALSE)
legend("topleft", c("Puget Sound"), pch=19, col="white", bty="n")
dev.off()

plot(dem, breaks=BreaksAll,col=cols4,ext = extD, legend=FALSE,axes=FALSE, bty="n" )


font_add("Raleway", "Raleway-Regular.ttf")

par(family="Raleway")
plot(c(0,1),c(0,1))

legend("topleft",c("test"), text.font=1,pch=19, col="white", bty="n")

#read in shapefiles ----
hydro <- readOGR("/Users/hkropp/Google Drive/GIS/vector/water/DNR_Hydrography_-_Water_Bodies-shp/DNR_Hydrography_-_Water_Bodies.shp")

hydro@proj4string
hyrop <- spTransform(hydro, dem@crs)



plot(dem, breaks=BreaksAll,col=cols5, legend=FALSE )
plot(hillL, col=grey(0:100/100), alpha=0.45,add=TRUE, legend=FALSE)

plot(hyrop, add = TRUE, col="black")
