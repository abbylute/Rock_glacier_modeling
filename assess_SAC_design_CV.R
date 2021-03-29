
# assess autocorrelation and design cross validation

library(blockCV)
library(tidyverse)
library(sf)
library(raster)
 
dir <- '/Volumes/WDPassport/Rock_glacier_research/WUS/Data/'


# Load presence, background, and domain data
#-----------------------------------------------------------------------
rg <- read_csv(paste0(dir,'Maxent_tables/rg_topo_geo.txt'))
bg <- read_csv(paste0(dir,'Maxent_tables/bg_topo_geo.txt'))
domain <- raster(paste0(dir,'Domain/rg_domain.tif'))
 
rgsf <- st_as_sf(rg, coords = c('lon','lat'), crs = crs(domain))
rgsf <- st_transform(rgsf, crs = "+proj=utm +zone=11 +ellps=GRS80 +units=m +no_defs ")

bgsf <- st_as_sf(bg, coords = c('lon','lat'), crs = crs(domain))
bgsf <- st_transform(bgsf, crs = "+proj=utm +zone=11 +ellps=GRS80 +units=m +no_defs ")

 

# Create presence-background SpatialPointsDataFrame
#-----------------------------------------------------------------------
#PB <- data.frame(cbind('Species' = 1, rg[,2:3]))
#PB <- rbind(PB, data.frame(cbind('Species' = 0, bg[,2:3])))
 
#pb_data <- st_as_sf(PB, coords = c('lon','lat'), crs = crs(domain)) 
rm(domain);gc();
 

 
# Load raster stack
#-----------------------------------------------------------------------
rr <- stack(paste0(mrdir,'maxent_variable_stack.tif'))
names(rr) <- c('aspect','slope','hw5','hw3','tmin','tmax','tmean','tschange','ppt',
               'swdown','sfe','maxswe','duration','nosnowdays')


# Assess spatial autocorrelation
#-----------------------------------------------------------------------
sampnum = 50000;
dev.off();gc()
quartz()
tic()
sac <- spatialAutoRange(rasterLayer = rr,
                        sampleNumber = sampnum,
                        doParallel = TRUE,
                        showPlots = TRUE)#,
toc()
saveRDS(sac, file = paste0(dir,'SAC/SAC_',sampnum,'.RData'))


sac1 <- readRDS(paste0(dir,'SAC/SAC_5000.RData'))
sac2 <- readRDS(paste0(dir,'SAC/SAC_10000.RData'))
sac3 <- readRDS(paste0(dir,'SAC/SAC_50000.RData'))


# I assessed spatial autocorrelation for different sample sizes. 
# they all suggested block sizes in teh range of 40,000-60,000m.
# This size seems too small, samples in CV might not be sufficiently independent.
# in all cases, swdown, ppt, and tmin had the highest sac, although
# their order was different depending on the sample size.

# save the figures for sample size 50000:
p3 <- sac3$plots
jpeg(paste0(dir,'SAC/barchart_50000.jpeg'),units = 'in', width = 5, height = 5, res = 400)
p3$barchart
dev.off()

jpeg(paste0(dir,'SAC/mapplot_50000.jpeg'),units = 'in', width = 5, height = 5, res = 400)
p3$mapplot
dev.off()


# Choose a larger range:
rangeExplorer(rasterLayer = rr)

# looks like ~200,000m might be good
blocksz = 400000


# Create blocks
#------------------------------------------------------------
# NOTE: if I use rgsf for 'speciesData', the blocks don't cover the full domain.
# using the entirety of bgsf takes over an hour to run. But sampling from bgsf
# seems to work well.
# k = 5 means training on ~80% of the data and testing on ~20% of the data
sb <- spatialBlock(speciesData = bgsf[seq(1,12721312,101),],
                   #species = "species",
                   rasterLayer = rr,
                   theRange = blocksz, # size of the blocks
                   k = 2, # number of folds
                   selection = "systematic",
                   iteration = 100, # find evenly dispersed folds
                   #numLimit = 50,
                   biomod2Format = FALSE,
                   xOffset = 0, # shift the blocks horizontally
                   yOffset = 0)

saveRDS(sb, file = paste0(dir,'CV/spatial_blocks_bs',format(blocksz, scientific=F),'.RData'))


foldExplorer(blocks = sb, rasterLayer = rr, speciesData = rgsf)
