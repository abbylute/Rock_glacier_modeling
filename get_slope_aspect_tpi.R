# Calculate terrain aspect and slope

library(raster)
outdir <- '/Volumes/WDPassport/Rock_glacier_research/WUS/Data/Topography/'
dem <- raster('/Volumes/WDPassport/DATA/DEM/NED/210m/WUS_NED_210m.tif')

# create slope raster:
terchar = terrain(dem, opt=c('slope','aspect','TPI'), unit='degrees', neighbors=8)
names(terchar)

slope <- terchar[[2]]
writeRaster(slope,paste0(outdir,'slope_210m_WUS.tif')) 

tpi <- terchar[[1]]
writeRaster(tpi,paste0(outdir,'tpi_210m_WUS.tif')) 

aspect <- terchar[[3]]
writeRaster(aspect,paste0(outdir,'aspect_210m_WUS.tif')) 



