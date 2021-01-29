
# Code to delineate areas to include in rock glacier modeling.

# need to pull in preindustrial temperature data (when it's ready) to further restrict the domain 


library(raster)
library(sf)
library(tidyverse)


##### Import Mountain Classes #####
mtn <- raster('/Volumes/WDPassport/DATA/Terrain/GME_K2classes/k2classes.tif')

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


##### Import Rock Glaciers #####
#rg <- st_read('/Volumes/WDPassport/Rock_glacier_research/Old/Data/Rock_glaciers/RG_Inventory_v2/RG.shp')
#rgll <- st_coordinates(rg)
#rgll <- rgll[,1:2]

rg <- read_delim('/Volumes/WDPassport/Rock_glacier_research/Old/Data/Rock_glaciers/RG_Inventory_v2/RG.FOI_Inventory.txt',' ')
rg %>% filter(type=='rockglacier') %>% group_by(activity) %>% count()
# for now, only use RGs of class 1
rg <- rg %>% filter(type=='rockglacier',activity==1) %>% select(lon,lat)

plot(mtn)
points(rg)


##### Which mountain classifications do rg's fall in? #####
mtn_locs <- raster::extract(mtn,rg)
unique(mtn_locs)
sum(is.na(mtn_locs))

rg[is.na(mtn_locs),]

# extracting the rg's directly from the coarse mtn layer results in 2 rgs falling in the NA mtn class.
# I think the original is about 4.5 km res,
mtn_fine <- disaggregate(mtn, 9, 'bilinear')
mtn_locs <- raster::extract(mtn_fine,rg)
#unique(mtn_locs)
sum(is.na(mtn_locs))
min(mtn_locs)
max(mtn_locs)
hist(mtn_locs)
sum(mtn_locs>14.5)

#If we use >15 as the threshold we'll get all RGs
# how much area would this be?
mtn_fine15 <- mtn_fine
#pb1[pb1 < 0] <- NA
mtn_fine15[mtn_fine15>15] <- NA
plot(mtn_fine15)
npix <- sum(!is.na(values(mtn_fine15)))/4 # number of square km to cover
n30 <- npix*(1000/30)*(1000/30) # number of 30m pixels
