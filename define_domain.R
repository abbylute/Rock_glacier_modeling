
# Code to delineate areas to include in rock glacier modeling.

# need to pull in preindustrial temperature data (when it's ready) to further restrict the domain 


library(raster)
library(sf)
library(tidyverse)
library(R.matlab)


##### Import Mountain Classes #####
mtn <- raster('/Volumes/WDPassport/DATA/Terrain/GME_K2classes/k2classes.tif')


##### Import Temperature #####
tavg <- readMat('/Volumes/WDPassport/DATA/WRF/Downscaled/summaries/PRE_mean_annual_tmean.mat')
tavg <- tavg$tt
ll <- raster('/Volumes/WDPassport/DATA/DEM/NED/new/WUS_NED_210m.tif')
values(ll) <- tavg
tavg <- ll; rm(ll); gc();


##### Import States #####
states <- st_read('Documents/IGERT/RockGlacierModeling/Data/Terrain/states_21basic/states.shp')
states <- st_transform(states, crs(mtn))


##### Crop mountains to WUS #####
states <- states[states$STATE_NAME %in% c('Washington','Oregon','California','Nevada','Arizona',
                                                   'New Mexico','Utah','Colorado','Idaho','Montana','Wyoming'),]
states_extent <- extent(states) + c(-.1,.1,-.1,.1) #create a broader extent to use for extractions 
mtn <- mask(mtn,states)
mtn <- crop(mtn,states_extent)
plot(mtn)

# classes:
# 11- nival
# 12- upper alpine
# 13- lower alpine
# 14- upper montane
# 15- lower montane


##### Crop temperature to WUS #####
tavg <- mask(tavg,states)
tavg <- crop(tavg,states_extent)
plot(tavg)


##### Make Mtn and Tavg rasters compatible #####
mtn <- projectRaster(mtn,tavg)


##### Import Rock Glaciers #####
#rg <- st_read('/Volumes/WDPassport/Rock_glacier_research/Old/Data/Rock_glaciers/RG_Inventory_v2/RG.shp')
#rgll <- st_coordinates(rg)
#rgll <- rgll[,1:2]

rg <- read_delim('/Volumes/WDPassport/Rock_glacier_research/Old/Data/Rock_glaciers/RG_Inventory_v2/RG.FOI_Inventory.txt',' ')
rg %>% filter(type=='rockglacier') %>% group_by(activity) %>% count()
# only use RGs of class 1 (active)
rg <- rg %>% filter(type=='rockglacier',activity==1) %>% select(lon,lat)
# save a table of active rock glacies
rg1 <- rg %>% mutate('species' = 'rockglacier') %>% select(species,lon,lat)
write_csv(rg1, '/Volumes/WDPassport/Rock_glacier_research/WUS/Data/Rock_glaciers/active_rgs.csv')

plot(mtn)
points(rg)


##### Which mountain classifications do rg's fall in? #####
mtn_locs <- raster::extract(mtn,rg)
#unique(mtn_locs)
sum(is.na(mtn_locs))

rg[is.na(mtn_locs),]

# extracting the rg's directly from the coarse mtn layer results in 2 rgs falling in the NA mtn class.
# I think the original is about 4.5 km res,
#mtn_fine <- disaggregate(mtn, 9, 'bilinear')
#mtn_fine <- focal(mtn_fine, w=matrix(1,5,5), mean)
#mtn_locs <- raster::extract(mtn_fine,rg)
mtn_locs <- raster::extract(mtn,rg)
#unique(mtn_locs)
sum(is.na(mtn_locs))
min(mtn_locs)
max(mtn_locs)
hist(mtn_locs)
sum(mtn_locs>=15)

#If we use >15 as the threshold we'll get all RGs
# how much area would this be?
#mtn_fine15 <- mtn_fine
mtn_fine15 <- mtn
mtn_fine15[mtn_fine15>=15] <- NA
plot(mtn_fine15)
#npix <- sum(!is.na(values(mtn_fine15)))/4 # number of square km to cover
#n210 <- npix*(1000/210)*(1000/210) # number of 210m pixels
n210 <- sum(!is.na(values(mtn_fine15)))

##### Then trim domain by temperature #####
mtn_locs <- raster::extract(tavg,rg)
sum(is.na(mtn_locs))
hist(mtn_locs)
max(mtn_locs)

tavg7 <- tavg
tavg7[tavg7>7] <- NA
plot(tavg7)


##### Combine temperature and mtn layers #####
rgdomain <- mask(mtn_fine15,tavg7)
plot(rgdomain)

# double check that all rock glaciers fall within this temperature/mtn domain:
mtn_locs <- raster::extract(rgdomain,rg)
sum(is.na(mtn_locs))
n210 <- sum(!is.na(values(rgdomain)))
# including temperature<8 hardly reduces the domain at all compared to mtn mask
# including temperature<7 reduces the number of pixels (relative to mtn mask) by ~23%


##### Save the domain mask #####
writeRaster(rgdomain,'/Volumes/WDPassport/Rock_glacier_research/WUS/Data/Domain/rg_domain.tif')

# also save lat/lons to import to matlab
xy <- coordinates(rgdomain)
val <- values(rgdomain)
f <- !is.na(val)
xy <- xy[f,]
xy <- data.frame(xy)
names(xy) <- c('lon','lat')
write_csv(xy,'/Volumes/WDPassport/Rock_glacier_research/WUS/Data/Domain/points_to_model.csv')


#library(rspatial)
#library(spData)
#library(sf)
library(ggplot2)
#us <- us_states
rgdf <- cbind(xy, val[f])

g1 <- ggplot() + 
  geom_tile(data=rgdf, aes(x=lon,y=lat)) +
  borders('state') +
  coord_cartesian(x=c(-125,-103), y = c(31,50)) + 
  theme_minimal()

jpeg('/Volumes/WDPassport/Rock_glacier_research/WUS/Data/Domain/modeling_domain.jpg',units='in',width=8,height=8,res=300)
g1 
dev.off()



