
# assess autocorrelation and design cross validation

# NOTE: this used to run on my laptop, then I updated some packages and now I can't get the 
# spatialAutoRange function to run. I had to run it on tesla instead.

library(blockCV)
library(tidyverse)
library(sf)
library(raster)
library(tictoc)

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

rm(domain);gc();
 

 
# Load raster stack
#-----------------------------------------------------------------------
rr <- stack(paste0(dir,'Masked_rasters/PRE/maxent_variable_stack_utm.tif'))
names(rr) <- c('aspect','slope','hw5','hw3','tmin','tmax','tmean','tschange','ppt',
               'swdown','sfe','maxswe','duration','nosnowdays','lith')

# remove lithology layer since categoricals are not allowed in SAC calculations
#rr <- rr[,,1:14]
# remove variables deemed too collinear
rr <- subset(rr, c(1:3,7:8,10:11,13:14))

#r1 <- crop(rr, extent(rr,2000,3050,2000,3050))

# Assess spatial autocorrelation
#-----------------------------------------------------------------------
sampnum = 10000;
#dev.off();gc()
quartz()
tic()
sac <- spatialAutoRange(rasterLayer = rr,
                        sampleNumber = sampnum,
                        doParallel = T,
                        showPlots = T)#,
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


# Create spatial blocks
#------------------------------------------------------------
# NOTE: if I use rgsf for 'speciesData', the blocks don't cover the full domain.
# using the entirety of bgsf takes over an hour to run. But sampling from bgsf
# seems to work well.
# k = 5 means training on ~80% of the data and testing on ~20% of the data
set.seed(17)
xx <- sample(1:dim(bgsf)[1], 10000, replace = F)

sb <- spatialBlock(speciesData = bgsf[xx,],
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


# Create environmental blocks based on MAT
#------------------------------------------------------------
rg <- read_csv(paste0(dir,'Maxent_tables/sample.txt'))
rg <- rg %>% dplyr::select(-c('hw3','tmin','tmax','ppt','maxswe'))
domain <- raster(paste0(dir,'Domain/rg_domain.tif'))

rgsf <- st_as_sf(rg, coords = c('lon','lat'), crs = crs(domain))
rgsf <- st_transform(rgsf, crs = "+proj=utm +zone=11 +ellps=GRS80 +units=m +no_defs ")
rm(domain);gc();

rr <- subset(rr, c(1:3,7:8,10:11,13:15))

eb <- envBlock(rasterLayer = rr$tmean,
               speciesData = rgsf[,c(1:6)], #bgsf[seq(1,12721312,101),],
               k = 2,
               standardization = 'normal',
               rasterBlock = F)
saveRDS(eb, file = paste0(dir,'CV/MAT_blocks.RData'))

fid <- as.character(eb$foldID)

rg2 <- cbind(rg, fid)

ggplot(rg2) +
  geom_point(aes(x=lon,y=lat,color=fid))

rglong <- rg2 %>% pivot_longer(cols=lon:nosnowdays, names_to='covar', values_to='value')

ggplot(rglong) +
  geom_density(aes(x=value,group=fid, color=fid)) +
  facet_wrap(~covar, scales='free')


               