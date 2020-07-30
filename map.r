
library(rcartocolor)
library(raster)
library(BAMMtools)

dirD <- "/Users/hkropp/Google Drive/GIS/WA_dem/WA_dem"

dem <- raster(paste0(dirD,"/psdem/psdem_2005.asc"))

plot(dem)

range(na.omit(getValues(dem)))
quantile(na.omit(getValues(dem)), prob=seq(0,1, by=0.05))

colWater <- rev(carto_pal(5,"Teal"))
colLand <- terrain.colors(9)
colLand2 <- c("#698474","#004643","#438a5e","#8cba51","#b0a160","#935900","#594a4e","#e5d8bf","white")
plot(seq(1,9), pch=19, col=colLand2)
display_carto_pal(6,"Teal")
watBreaks <- c(-1000,-600,-400,-200,-100,-50,-1)
landBreaks <-c(0,20,1000,1500,2000,2500,3000,4000,7500,13000) 
BreaksAll <- c(watBreaks,landBreaks)
cols <- c(colWater, colLand2)

plot(dem, breaks=BreaksAll,col=cols )

