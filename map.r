library(raster)

dem <- raster("c:\\Users\\hkropp\\Google Drive\\WA_dem\\psdem\\psdem_2005.asc")

plot(dem)

range(na.omit(getValues(dem)))
