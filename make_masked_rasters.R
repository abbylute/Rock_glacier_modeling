# Create masked raster for each variable

library(raster)
library(R.matlab)

dir <- '/Volumes/WDPassport/Rock_glacier_research/WUS/Data/'
outdir <- paste0(dir,'Masked_rasters/')

domain <- raster(paste0(dir,'Domain/rg_domain.tif'))
dem <- raster('/Volumes/WDPassport/DATA/DEM/NED/new/WUS_NED_210m.tif')

#bg <- read_csv(paste0(dir,'Maxent_tables/bg_topo_geo.txt'))

#ind <- readMat(paste0(dir,'Domain/index_bg_points_in_grid.mat'))
#icol = as.vector(ind$bg.col);
#irow = as.vector(ind$bg.row);
#rm(ind);gc();

# get cell numbers of background locations
#cnum <- cellFromRowCol(domain, irow, icol)
#rm(icol, irow); gc();


# lithology
lith <- raster('/Volumes/WDPassport/DATA/Geology/Lithology/Generalizedlith/WUS_rocktypes_210m.tif')
lith <- crop(lith, domain)
lith <- mask(lith, domain)
writeRaster(lith, paste0(outdir,'lithology.tif'))
rm(lith);gc();

# aspect
asp <- raster(paste0(dir,'Topography/aspect_210m_WUS.tif'))
asp <- crop(asp, domain)
asp <- mask(asp, domain)
writeRaster(asp, paste0(outdir,'aspect.tif'))
rm(asp);gc();

# slope
slo <- raster(paste0(dir,'Topography/slope_210m_WUS.tif'))
slo <- crop(slo, domain)
slo <- mask(slo, domain)
writeRaster(slo, paste0(outdir,'slope.tif'))
rm(slo);gc();

# headwall
hw5 <- readMat(paste0(dir,'Topography/headwall_WUS_210m.mat'))
hw5 <- hw5$hw
hw55 <- dem
values(hw55) <- hw5
hw55 <- crop(hw55, domain)
hw55 <- mask(hw55, domain)
writeRaster(hw55, paste0(outdir,'headwall5.tif'))
rm(hw5, hw55); gc();

hw5 <- readMat(paste0(dir,'Topography/headwall_WUS_210m_3x3.mat'))
hw5 <- hw5$hw
hw55 <- dem
values(hw55) <- hw5
hw55 <- crop(hw55, domain)
hw55 <- mask(hw55, domain)
writeRaster(hw55, paste0(outdir,'headwall3.tif'))
rm(hw5, hw55); gc();

# temperatures
tt <- readMat(paste0(dir,'Climate/tmin.mat'))
tt <- tt$tmin
tt1 <- dem
values(tt1) <- tt
tt1 <- crop(tt1, domain)
tt1 <- mask(tt1, domain)
writeRaster(tt1, paste0(outdir,'tmin.tif'))
rm(tt, tt1);gc();

tt <- readMat(paste0(dir,'Climate/tmax.mat'))
tt <- tt$tmax
tt1 <- dem
values(tt1) <- tt
tt1 <- crop(tt1, domain)
tt1 <- mask(tt1, domain)
writeRaster(tt1, paste0(outdir,'tmax.tif'))
rm(tt, tt1);gc();

tt <- readMat(paste0(dir,'Climate/tmean.mat'))
tt <- tt$tmean
tt1 <- dem
values(tt1) <- tt
tt1 <- crop(tt1, domain)
tt1 <- mask(tt1, domain)
writeRaster(tt1, paste0(outdir,'tmean.tif'))
rm(tt, tt1);gc();

tt <- readMat(paste0(dir,'Climate/tschange.mat'))
tt <- tt$tt
tt1 <- dem
values(tt1) <- tt
tt1 <- crop(tt1, domain)
tt1 <- mask(tt1, domain)
writeRaster(tt1, paste0(outdir,'tschange.tif'))
rm(tt, tt1);gc();

# precipitation
tt <- readMat(paste0(dir,'Climate/ppt.mat'))
tt <- tt$ppt
tt1 <- dem
values(tt1) <- tt
tt1 <- crop(tt1, domain)
tt1 <- mask(tt1, domain)
writeRaster(tt1, paste0(outdir,'ppt.tif'))
rm(tt, tt1);gc();

# swdown
tt <- readMat(paste0(dir,'Climate/swdown.mat'))
tt <- tt$sw
tt1 <- dem
values(tt1) <- tt
tt1 <- crop(tt1, domain)
tt1 <- mask(tt1, domain)
writeRaster(tt1, paste0(outdir,'swdown.tif'))
rm(tt, tt1);gc();

# sfe
tt <- readMat(paste0(dir,'Snow/sfe.mat'))
tt <- tt$tt
tt1 <- dem
values(tt1) <- tt
tt1 <- crop(tt1, domain)
tt1 <- mask(tt1, domain)
writeRaster(tt1, paste0(outdir,'sfe.tif'))
rm(tt, tt1);gc();

# maxswe
tt <- readMat(paste0(dir,'Snow/maxswe.mat'))
tt <- tt$tt
tt1 <- dem
values(tt1) <- tt
tt1 <- crop(tt1, domain)
tt1 <- mask(tt1, domain)
writeRaster(tt1, paste0(outdir,'maxswe.tif'))
rm(tt, tt1);gc();

# duration
tt <- readMat(paste0(dir,'Snow/duration.mat'))
tt <- tt$tt
tt1 <- dem
values(tt1) <- tt
tt1 <- crop(tt1, domain)
tt1 <- mask(tt1, domain)
writeRaster(tt1, paste0(outdir,'duration.tif'))
rm(tt, tt1);gc();

# nosnowdays
tt <- readMat(paste0(dir,'Snow/nosnowdays.mat'))
tt <- tt$tt
tt1 <- dem
values(tt1) <- tt
tt1 <- crop(tt1, domain)
tt1 <- mask(tt1, domain)
writeRaster(tt1, paste0(outdir,'nosnowdays.tif'))
rm(tt, tt1);gc();




mrdir <- paste0(dir,'Masked_rasters/')

rr <- stack(raster(paste0(mrdir,'aspect.tif')))
rr[[2]] <- raster(paste0(mrdir,'slope.tif'))
rr[[3]] <- raster(paste0(mrdir,'headwall5.tif'))
rr[[4]] <- raster(paste0(mrdir,'headwall3.tif'))
rr[[5]] <- raster(paste0(mrdir,'tmin.tif'))
rr[[6]] <- raster(paste0(mrdir,'tmax.tif'))
rr[[7]] <- raster(paste0(mrdir,'tmean.tif'))
rr[[8]] <- raster(paste0(mrdir,'tschange.tif'))
rr[[9]] <- raster(paste0(mrdir,'ppt.tif'))
rr[[10]] <- raster(paste0(mrdir,'swdown.tif'))
rr[[11]] <- raster(paste0(mrdir,'sfe.tif'))
rr[[12]] <- raster(paste0(mrdir,'maxswe.tif'))
rr[[13]] <- raster(paste0(mrdir,'duration.tif'))
rr[[14]] <- raster(paste0(mrdir,'nosnowdays.tif'))
rr[[15]] <- raster(paste0(mrdir,'lithology.tif'))

# save longlat version
writeRaster(rr, paste0(mrdir,'maxent_variable_stack_longlat.tif'))

# project to UTM
rr <- projectRaster(rr, crs = "+proj=utm +zone=11 +ellps=GRS80 +units=m +no_defs ")

# save utm version
writeRaster(rr, paste0(mrdir,'maxent_variable_stack_utm.tif'))

# DELETE:
#rr <- stack(paste0(mrdir,'maxent_variable_stack.tif'))
#lith <- raster(paste0(mrdir,'lithology.tif'))
#lith <- projectRaster(lith, crs = "+proj=utm +zone=11 +ellps=GRS80 +units=m +no_defs ")
#rr[[15]] <- lith
#writeRaster(rr, paste0(mrdir,'maxent_variable_stack_utm.tif'))

