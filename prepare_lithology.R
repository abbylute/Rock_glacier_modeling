library(sf)
library(fasterize)
library(raster)
library(R.matlab)

ss <- st_read('/Volumes/WDPassport/DATA/Geology/Lithology/Generalizedlith/geol_poly_albers_anning/geol_poly_albers_anning.shp')

# NOTE: this takes forever to plot
# ggplot() + geom_sf(data=ss)
# looks like this 12 category lithology layer would be a good option for us, it has relevant categories but not 
# too many of them, it covers the domain we need. Just need to translate it from shapefile to 210m raster.

# reproject shapefile to lat lon coordinates
sst <- st_transform(ss, 4326)
#ggplot() + geom_sf(data = sst)


# Rasterize
rr <- raster('/Volumes/WDPassport/DATA/DEM/NED/new/WUS_NED_210m.tif')#sst) # raster template
plot(rr)
rs <- fasterize(sst, rr, field = "rocktype")
plot(rs)

writeRaster(rs,'/Volumes/WDPassport/DATA/Geology/Lithology/Generalizedlith/WUS_rocktypes_210m.tif')

# also save as matfile
xy <- coordinates(rs)
lith <- values(rs)

writeMat('/Volumes/WDPassport/DATA/Geology/Lithology/Generalizedlith/WUS_rocktypes_210m.mat', xy=xy, lith=lith)


