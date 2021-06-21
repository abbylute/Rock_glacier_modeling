

library(raster)
library(tidyverse)

rr <- raster('WUS/Data/Domain/USGS_13_n46w110.tif')
rr <- crop(rr, extent(-109.7, -109.53, 45.04, 45.18))


slope <- terrain(rr, opt='slope')
aspect <- terrain(rr, opt='aspect')

# create hillshade
# numbers 
hill <- hillShade(slope, aspect, 
                  angle=10, 
                  direction=0)

plot(hill,
     col=grey.colors(100, start=0, end=1),
     legend=F)
# overlay CHM on top of hillshade
plot(rr,
     add=T,
     alpha=.5)

writeRaster(hill,'WUS/Data/Domain/Beartooth_hillshade.tif')


hilldf <- data.frame(coordinates(hill), values(hill))
names(hilldf) <- c('x','y','val')
write_csv(hilldf,'WUS/Data/Domain/Beartooth_hillshade.csv')


