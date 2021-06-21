
# 1. Create raster of locations that enhanced between pre and ctrl
dir1 <- 'WUS/Data/Maxent_outputs/May-26-2021/'
pred_pre <- raster(paste0(dir1,'preindustrial_predictions.tif'))
pred_ctrl <- raster(paste0(dir1,'ctrl_predictions.tif'))

en <- pred_pre
values(en) <- NA
en[pred_pre<.212& pred_ctrl>.212] <- 1
plot(en)

# 2. load glacier locations
glims <- st_read('/Volumes/WDPassport/DATA/GLIMS/glims_polygons.shp')

# 3. create dataframe of enhance locations
endf <- data.frame(coordinates(en), values(en))
names(endf) <- c('x','y','en')
endf <- endf %>% filter(!is.na(en))
endf <- st_as_sf(endf, coords = c('x','y'), crs = st_crs(glims))

# 4. check for overlap with glacier locations
ov <- st_intersection(endf, glims)
print(paste0(nrow(ov), ' out of ',nrow(endf), ' locations that enhanced were collocated with glaciers'))
 
