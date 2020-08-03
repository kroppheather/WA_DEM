library(rgdal)
library(rcartocolor)
library(raster)
library(extrafont)
library(sysfonts)

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



#colorful mapping ----
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

textA <- strwrap("Combined bathymetry and topography of the Puget Lowlands, 
             Washington State (January 2005). David Finlayson.",
                 width=40)

png(paste0(dirOut, "/WA_dem.png"), height = 30, width = 20, units = "in", res=300)

par(family="Raleway",mai=c(3,2,3,2))
plot(dem, breaks=BreaksAll,col=cols4,maxpixels = 13000000, ext = extD, legend=FALSE,axes=FALSE, bty="n", box = FALSE )

plot(hillL, col=grey(0:100/100), alpha=0.15,maxpixels = 13000000, ext = extD, add=TRUE, legend=FALSE, box = FALSE)
legend("topleft", c(" "), title= "Puget Sound", cex =5.25, pch=19, 
       col="#dddddd",bg="#dddddd", box.col="#dddddd", title.col = "#2c7873")
for(i in 1:length(textA)){
  text(1005000,501000-(i*7000),textA[i], cex=1.6, col="#ba7967",adj = c(0,0))
}
dev.off()

#second color scheme mapping ----
#set up colors
colWater <- rev(carto_pal(6,"Teal"))


colLandb <- c(colWater[6],#water transition
              "azure1",#lowland wetland azure1"
              "antiquewhite3", #forest
              "darkseagreen3",#higher elevation
              "darkseagreen3",#vegetation to treeline
              "darkseagreen4",#high elev 1
              "seashell3",#high elev 2
              "lavenderblush3",#high elev 3
              "white")#mountain top


plot(seq(1,9), pch=19, col=colLandb)
#set up breaks
watBreaks <- c(-1000,-600,-400,-200,-100,-50,-1)
landBreaks <-c(0,100,500,1200,1800,3800,4500,5500,13000) 
BreaksAll <- c(watBreaks,landBreaks)

colsb <- c(colWater, colLandb)

png(paste0(dirOut, "/WA_dem_dull.png"), height = 30, width = 20, units = "in", res=300)

par(family="Raleway",mai=c(3,2,3,2))
plot(dem, breaks=BreaksAll,col=colsb, ext = extD, maxpixels = 13000000, legend=FALSE,axes=FALSE, bty="n", box = FALSE )

plot(hillL, col=grey(0:100/100), alpha=0.55, ext = extD,maxpixels = 13000000,  add=TRUE, legend=FALSE, box = FALSE)
legend("topleft", c(" "), title= "Puget Sound", cex =5.25, pch=19, 
       col="azure1",bg="azure1", box.col="azure1", title.col = "darkseagreen4")
for(i in 1:length(textA)){
  text(1005000,501000-(i*7000),textA[i], cex=1.6, col="darkseagreen3",adj = c(0,0))
}
dev.off()

#read in shapefiles ----
hydro <- readOGR("/Users/hkropp/Google Drive/GIS/vector/water/DNR_Hydrography_-_Water_Bodies-shp/DNR_Hydrography_-_Water_Bodies.shp")

hydro@proj4string
hyrop <- spTransform(hydro, dem@crs)
head(hyrop@data)
unique(hyrop@data$WB_HYDR_FT)

glac <- hyrop[hyrop@data$WB_HYDR_FT == "Glacier",]
plot(glac)

river <- hyrop[hyrop@data$WB_HYDR_FT == "Stream",]

lakes <- hyrop[hyrop@data$WB_HYDR_FT == "Lake",]

glacC <- crop(glac,dem)
plot(glacC)

lakesC <- crop(lakes,dem)
plot(lakesC)


plot(dem, breaks=BreaksAll,col=cols5, legend=FALSE )
plot(hillL, col=grey(0:100/100), alpha=0.45,add=TRUE, legend=FALSE)

plot(hyrop, add = TRUE, col="black")
