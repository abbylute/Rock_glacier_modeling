
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
# 16- remaining mtn areas with frost
# 17- remaining mtn areas without frost


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

tavg8 <- tavg
tavg8[tavg8>8] <- NA
plot(tavg8)


##### Combine temperature and mtn layers #####
rgdomain <- mask(mtn_fine15,tavg8)
plot(rgdomain)

# double check that all rock glaciers fall within this temperature/mtn domain:
mtn_locs <- raster::extract(rgdomain,rg)
sum(is.na(mtn_locs))
n210 <- sum(!is.na(values(rgdomain)))
# including temperature<8 hardly reduces the domain at all compared to mtn mask
# including temperature<7 reduces the number of pixels (relative to mtn mask) by ~23%


##### See if domain covers Millar's Sierra Nevada RGs #####
mi <- read_csv('/Users/abbylute/Documents/IGERT/RockGlacierModeling/Data/Glacier/Millar_SierraNevada_RIF_Inventory/RIFDatabase_NSIDC.csv',
               skip =3)
mi <- mi %>% filter(Activity == 'M') # only modern features, not relict
mi_locs <- raster::extract(rgdomain, mi[,c(4,3)])
sum(is.na(mi_locs))
plot(rgdomain)
points(mi[,c(4,3)])

##### See if domain covers Kinworthy's New Mexico RGs #####
# their rg classification looks for features with vegetation and with thaw features, suggesting that many of 
# these are relicts. the warmest one has preindustrial MAT of 12.7C! probably not worth including these.
#nm <- read_csv('/Volumes/WDPassport/Data/Rock_glaciers/Kinworthy_NM_inventory/Kinworthy_NM_inventory.csv')
#nm_locs <- raster::extract(rgdomain, nm[,c(3,2)])
#sum(is.na(nm_locs))


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
