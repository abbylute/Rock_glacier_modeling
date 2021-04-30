# Create masked raster for each variable

library(raster)
library(R.matlab)

era <- 'PGW'
dir <- '/Volumes/WDPassport/Rock_glacier_research/WUS/Data/'
outdir <- paste0(dir,'Masked_rasters/',era,'/')

domain <- raster(paste0(dir,'Domain/rg_domain.tif'))
dem <- raster('/Volumes/WDPassport/DATA/DEM/NED/new/WUS_NED_210m.tif')


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
tt <- readMat(paste0(dir,'Climate/',era,'/tmin.mat'))
tt <- tt$tmin
tt1 <- dem
values(tt1) <- tt
tt1 <- crop(tt1, domain)
tt1 <- mask(tt1, domain)
writeRaster(tt1, paste0(outdir,'tmin.tif'))
rm(tt, tt1);gc();

tt <- readMat(paste0(dir,'Climate/',era,'/tmax.mat'))
tt <- tt$tmax
tt1 <- dem
values(tt1) <- tt
tt1 <- crop(tt1, domain)
tt1 <- mask(tt1, domain)
writeRaster(tt1, paste0(outdir,'tmax.tif'))
rm(tt, tt1);gc();

tt <- readMat(paste0(dir,'Climate/',era,'/tmean.mat'))
tt <- tt$tmean
tt1 <- dem
values(tt1) <- tt
tt1 <- crop(tt1, domain)
tt1 <- mask(tt1, domain)
writeRaster(tt1, paste0(outdir,'tmean.tif'))
rm(tt, tt1);gc();

tt <- readMat(paste0(dir,'Climate/',era,'/tschange.mat'))
tt <- tt$tt
tt1 <- dem
values(tt1) <- tt
tt1 <- crop(tt1, domain)
tt1 <- mask(tt1, domain)
writeRaster(tt1, paste0(outdir,'tschange.tif'))
rm(tt, tt1);gc();

# precipitation
tt <- readMat(paste0(dir,'Climate/',era,'/ppt.mat'))
tt <- tt$ppt
tt1 <- dem
values(tt1) <- tt
tt1 <- crop(tt1, domain)
tt1 <- mask(tt1, domain)
writeRaster(tt1, paste0(outdir,'ppt.tif'))
rm(tt, tt1);gc();

# swdown
tt <- readMat(paste0(dir,'Climate/',era,'/swdown.mat'))
tt <- tt$sw
tt1 <- dem
values(tt1) <- tt
tt1 <- crop(tt1, domain)
tt1 <- mask(tt1, domain)
writeRaster(tt1, paste0(outdir,'swdown.tif'))
rm(tt, tt1);gc();

# sfe
tt <- readMat(paste0(dir,'Snow/',era,'/sfe.mat'))
tt <- tt$tt
tt1 <- dem
values(tt1) <- tt
tt1 <- crop(tt1, domain)
tt1 <- mask(tt1, domain)
writeRaster(tt1, paste0(outdir,'sfe.tif'))
rm(tt, tt1);gc();

# maxswe
tt <- readMat(paste0(dir,'Snow/',era,'/maxswe.mat'))
tt <- tt$tt
tt1 <- dem
values(tt1) <- tt
tt1 <- crop(tt1, domain)
tt1 <- mask(tt1, domain)
writeRaster(tt1, paste0(outdir,'maxswe.tif'))
rm(tt, tt1);gc();

# duration
tt <- readMat(paste0(dir,'Snow/',era,'/duration.mat'))
tt <- tt$tt
tt1 <- dem
values(tt1) <- tt
tt1 <- crop(tt1, domain)
tt1 <- mask(tt1, domain)
writeRaster(tt1, paste0(outdir,'duration.tif'))
rm(tt, tt1);gc();

# nosnowdays
tt <- readMat(paste0(dir,'Snow/',era,'/nosnowdays.mat'))
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
rr[[5]] <- raster(paste0(mrdir,era,'/tmin.tif'))
rr[[6]] <- raster(paste0(mrdir,era,'/tmax.tif'))
rr[[7]] <- raster(paste0(mrdir,era,'/tmean.tif'))
rr[[8]] <- raster(paste0(mrdir,era,'/tschange.tif'))
rr[[9]] <- raster(paste0(mrdir,era,'/ppt.tif'))
rr[[10]] <- raster(paste0(mrdir,era,'/swdown.tif'))
rr[[11]] <- raster(paste0(mrdir,era,'/sfe.tif'))
rr[[12]] <- raster(paste0(mrdir,era,'/maxswe.tif'))
rr[[13]] <- raster(paste0(mrdir,era,'/duration.tif'))
rr[[14]] <- raster(paste0(mrdir,era,'/nosnowdays.tif'))
rr[[15]] <- raster(paste0(mrdir,'lithology.tif'))

# save longlat version
writeRaster(rr, paste0(mrdir,era,'/maxent_variable_stack_longlat.tif'))

# project to UTM
rr <- projectRaster(rr, crs = "+proj=utm +zone=11 +ellps=GRS80 +units=m +no_defs ")

# save utm version
writeRaster(rr, paste0(mrdir,era,'/maxent_variable_stack_utm.tif'))

