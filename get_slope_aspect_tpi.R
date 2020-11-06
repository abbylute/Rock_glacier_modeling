# Calculate terrain aspect and slope

library(raster)
outdir <- '/Volumes/WDPassport/Rock_glacier_research/WUS/Data/Topography/'
dem <- raster('/Volumes/WDPassport/DATA/DEM/NED/210m/WUS_NED_210m.tif')
source('/Volumes/WDPassport/Rock_glacier_research/WUS/Code/calc_headwall.R')

# create slope raster:
terchar = terrain(dem, opt=c('slope','aspect','TPI'), unit='degrees', neighbors=8)
names(terchar)

slope <- terchar[[2]]
writeRaster(slope,paste0(outdir,'slope_210m_WUS.tif')) 

tpi <- terchar[[1]]
writeRaster(tpi,paste0(outdir,'tpi_210m_WUS.tif')) 

aspect <- terchar[[3]]
writeRaster(aspect,paste0(outdir,'aspect_210m_WUS.tif')) 



# calculate headwall metric
buf = 400 #radius in meters
slope_thres = 30 # threshold slope in degrees
i = 1:5000000;#length(values(dem))
hw <- calc_headwall(iter=i,demsmall2=dem,slope=slope,buf=buf,slope_thres=slope_thres)


# old code
use <- which(!is.na(values(demsmall2)))
use <- matrix(use,ncol=1)

# may need to separate this into chunks to get it to run:
#use1 <- use[1:3000000]
#use1 <- use[3000001:6000000]
#use1 <- use[6000001:length(use)]
# if chunking not needed, then:
use1 <- use

# set up
use1 <- matrix(use1,nrow=length(use1),ncol=1)

# running 3million cells takes about 9.7 hours
cl <- makeCluster(clnum)
clusterExport(cl, c("calc_headwall","use1","buf","slope","slope_thres","demsmall2"))
tic(paste0('calculate headwall metric'))
headwall_metric_9mil <- parApply(cl,use1, 1, function(i) calc_headwall(iter=i,demsmall2=demsmall2,slope=slope,buf=buf,slope_thres=slope_thres))
toc()
stopCluster(cl)

