# Calculate terrain aspect and slope

library(raster)
library(parallel)
library(R.matlab)
clnum <- detectCores()-2

mdir <- '/Volumes/WDPassport/'
outdir <- paste0(mdir,'Rock_glacier_research/WUS/Data/Topography/')
dem <- raster('/Volumes/WDPassport/DATA/DEM/NED/new/WUS_NED_210m.tif')

#source(paste0(mdir,'Rock_glacier_research/WUS/Code/calc_headwall.R'))

# create slope raster:
terchar = terrain(dem, opt=c('slope','aspect','TPI'), unit='degrees', neighbors=8)
names(terchar)

slope <- terchar[[2]]
writeRaster(slope,paste0(outdir,'slope_210m_WUS.tif')) 

tpi <- terchar[[1]]
writeRaster(tpi,paste0(outdir,'tpi_210m_WUS.tif')) 

aspect <- terchar[[3]]
writeRaster(aspect,paste0(outdir,'aspect_210m_WUS.tif')) 


# Save slope, aspect, tpi as matfiles as well
xy <- coordinates(dem)
slope <- values(slope)
aspect <- values(aspect)
tpi <- values(tpi)

writeMat(paste0(outdir,'slope_210m_WUS.mat'), 'xy'=xy,'slope'=slope)
writeMat(paste0(outdir,'aspect_210m_WUS.mat'), 'xy'=xy,'aspect'=aspect)
writeMat(paste0(outdir,'tpi_210m_WUS.mat'), 'xy'=xy,'tpi'=tpi)



# headwall metric is better calculated in Matlab get_headwall.m. Way faster and don't have to chunk.
# calculate headwall metric
#buf = 400 #radius in meters
#slope_thres = 30 # threshold slope in degrees

#use <- which(!is.na(values(dem)))
#use <- matrix(use,ncol=1)
#latlon <- coordinates(dem)
#latlon <- latlon[!is.na(values(dem)),]

#chunksize = 5000000
#nc <- ceiling(length(use)/chunksize)

# preallocate output
#hwtab <- data.frame(lon=numeric(),lat=numeric(),hw=numeric())

#for (ll in 1:nc){
#  print(paste0('running group ',as.character(ll),' of ', as.character(nc)))
#  st = (ll-1)*chunksize+1
#  en = min(ll*chunksize, length(use))

#  use1 <- st:en
#  use1 <- matrix(use1,nrow=length(use1),ncol=1)

  # running 3million cells takes about 9.7 hours
#  cl <- makeCluster(clnum)
#  clusterExport(cl, c("calc_headwall","use1","buf","slope","slope_thres","dem"))
  #tic(paste0('calculate headwall metric'))
#  headwall_metric <- parApply(cl,use1, 1, function(i) calc_headwall(iter=i,demsmall2=dem,slope=slope,buf=buf,slope_thres=slope_thres))
  
  # add to large output dataframe
#  hnew <- data.frame(latlon[st:en,],headwall_metric)
#  names(hnew) <- c('lon','lat','hw')
#  hwtab <- rbind(hwtab, hnew)
  
  #write.table(headwall_metric,paste0(outdir,'headwall/headwall_metric_group_',ll,'.csv'))
  #toc()
#  stopCluster(cl)

#}

#write.table(hwtab,paste0(outdir,'headwall_metric.csv'))

