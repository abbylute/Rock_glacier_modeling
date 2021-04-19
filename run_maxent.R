# Maxent Modeling of Rock Glaciers

# on crc servers:
dyn.load("/opt/modules/climatology/gdal/3.0.2/lib/libgdal.so")


library(rJava)
library(tictoc)
library(sf) #library(sf,lib='/mnt/ceph/alute/Rpackages/') 
library(tidyverse)
library(blockCV)
library(parallel)
#library(dismo)
library(ENMeval) # vignette: https://cran.r-project.org/web/packages/ENMeval/vignettes/ENMeval-vignette.html
source('WUS/Code/ENMevaluate_al.R')
source('WUS/Code/tuning_al.R')
#source('WUS/Code/aicc_cobos.R')
source('WUS/Code/modelTune.maxentJar_al.R')
source('WUS/Code/calc.aicc_al.R')

indir <- 'WUS/Data/Maxent_tables/'

outdir <- paste0('WUS/Data/Maxent_outputs/',format(Sys.time(), "%b-%d-%Y"),'/')

if (!dir.exists(outdir)){
  dir.create(outdir)
}

# import data
#---------------------------------------------
samp <- read_csv(paste0(indir,'sample.txt'))
print('loading background')
bgr <- read_csv(paste0(indir,'background.txt'))

# randomly sample 10k of background
# (should these be sample randomly, or based on distance from presences as in Hijmans, 2012?)
set.seed(17)
xx <- sample(1:dim(bgr)[1], 10000, replace = F)
bgr <- bgr[xx,]


# create groups for cross validation
#---------------------------------------------
#blocks <- get.block(samp[,2:3], bgr[,2:3])
#plot(bgr[,2:3], col='gray')
#points(samp[,2:3], pch=21, bg=blocks$occ.grp)

# environmental blocks:
s2 <- samp %>% mutate(fold = if_else(tmean>median(tmean),1,2))
bgfold <- bgr %>% mutate(fold = if_else(tmean>median(samp$tmean),1,2))
bgfold <- as.numeric(bgfold$fold)

# will this require too much extrapolation? Could do user-defined groups instead.
blocks <- readRDS('WUS/Data/CV/spatial_blocks_bs400000.RData')
bl <- blocks$blocks

bl <- st_as_sf(bl)
#bgsf <- st_as_sf(bgr[,2:3], coords = c('lon','lat'), crs = st_crs(bl))
#rgsf <- st_as_sf(samp[,2:3], coords = c('lon','lat'), crs = st_crs(bl))

#bgfold <- st_join(bl, bgsf)
#rgfold <- st_join(bl, rgsf)

#st_geometry(bgfold) = NULL
#st_geometry(rgfold) = NULL

# new approach
bgfold <- blocks$foldID
#bl <- blocks$blocks
rgsf <- st_as_sf(samp[,2:3], coords = c('lon','lat'), crs = '+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs')
rgsf <- st_transform(rgsf, crs(bl))
out <- st_intersects(rgsf, bl)

ff = rep(c(1,2),12)
s2 <- as.data.frame(cbind(st_coordinates(rgsf),unlist(out)))
names(s2) <- c('lon','lat','grp')
s2$fold <- ff[s2$grp]
s2$grp <- as.character(s2$grp)
ggplot(s2)+
  geom_sf(data=bl) +
geom_point(data=s2,aes(x=lon,y=lat,color=fold)) 


# remove collinear variables
#--------------------------------------------
# variables to remove:
# 3x3 window headwall metric, tmin, tmax, maximum SWE, and precipitation
not_use <- c('hw3','tmin','tmax','maxswe','ppt')
#not_use <- c('hw3','tmin','tmax','maxswe','ppt','sfe','duration','maxswe','nosnowdays')
samp <- samp %>% dplyr::select(-not_use)
bgr <- bgr %>% dplyr::select(-not_use)



# run maxent  NOTE: AIC IS CALCULATED WRONG IN THIS METHOD (_al), but other method doesn't allow outdir
#---------------------------------------------
# occ and bg.coords should be dataframes that start with "LON" and "LAT" columns
# cannot use checkboard cross validation options for now
occ <- as.data.frame(samp[,2:dim(samp)[2]])
names(occ)[1:2] <- c("LON","LAT")
bg.coords = as.data.frame(bgr[,2:dim(bgr)[2]])
names(bg.coords)[1:2] <- c("LON","LAT")
betas <- seq(1,101,10)#c(1,2,4,6,8,10)
feats <- c('L','LQ','LQP','LQPH','LQPHT')
tic('running maxent took ')
eval1 <- ENMevaluate_al(occ=occ, 
                        env=NULL, 
                        bg.coords=bg.coords,
                        occ.grp = s2$fold, #rgfold$folds,#
                        bg.grp = bgfold, #bgfold$folds,#
                        rasterPreds=TRUE, 
                        method='user', 
                        #method='block', 
                        RMvalues=betas, 
                        fc=feats, 
                        clamp=TRUE,
                        categoricals='lith', 
                        algorithm='maxent.jar', 
                        jackknife=TRUE, 
                        #parallel=TRUE,
                        bin.output=TRUE,
                        outdir=outdir) 
toc()


saveRDS(eval1, file=paste0(outdir,'ENMeval_object.RData'))




# can read in .dat files using read_csv

# currently cannot calculate niche overlap because predictions are not raster, may be possible to modify the fucntion to do this though
eval1@models # html
eval1@results # training and test AUCs, AICs

response(eval1@models[[2]]) # response curves (number is the betas/feats combo numbere)

dismo::pwdSample() # does pair-wise distance sampling as described in Hijmans, 2012
dismo::ssb() # assesses spatial sorting bias as discussed in Hijmans, 2012







# alternate approach using raster and ENMeval original code:
#rr <- stack('WUS/Data/Masked_rasters/maxent_variable_stack.tif')
#names(rr) <- c('aspect','slope','hw5','hw3','tmin','tmax','tmean','tschange','ppt',
               'swdown','sfe','maxswe','duration','nosnowdays')

#dir <- '/Volumes/WDPassport/Rock_glacier_research/WUS/Data/'
#domain <- raster(paste0(dir,'Domain/rg_domain.tif'))

#rr <- subset(rr, c(1:3,7:8,10:11,13:14))
#rr <- projectRaster(rr, domain)
#writeRaster(rr,paste0(dir,'Masked_rasters/maxent_variable_stack_trimmed_longlat.tif'))
#rm(dir,domain);gc();

# prepare raster:
dir <- 'WUS/Data/Masked_rasters/'
rr <- stack(paste0(dir,'maxent_variable_stack_longlat.tif'));
names(rr) <- c('aspect','slope','hw5','hw3','tmin','tmax','tmean','tschange','ppt',
               'swdown','sfe','maxswe','duration','nosnowdays','lith')
rr <- subset(rr, c(1:3,7:8,10:11,13:15))


#samp <- samp[,1:7]
#bg.coords <- bg.coords[,1:7]
betas <- c(1,1.5,2,3,4,6,8,10)#c(1,2,4,6,8,10)
feats <- c('L','LQ','LQP','LQPH','LQPHT')


eval1 <- ENMevaluate(occ=occ[,1:2], 
                        env=rr, 
                        bg.coords=bg.coords[,1:2],
                        occ.grp = s2$fold,
                        bg.grp = bgfold,
                        rasterPreds=TRUE, 
                        method='user', #method='block', 
                        RMvalues=betas, 
                        fc=feats, 
                        clamp=TRUE,
                        categoricals='lith', 
                        algorithm='maxent.jar')#, 
                        #jackknife=TRUE, 
                        #outdir=outdir) 
saveRDS(eval1, file=paste0(outdir,'ENMeval_object.RData'))

# make predictions across preindustrial domain:
predfn <- paste0(outdir,'preindustrial_predictions.tif')

mx = eval1@models[[1]]
pp <- predict(mx, rr, ext=NULL, filename=outfn, progress='text')#, args="outputformat=cloglog")
# the default outputformat from the predict function is cloglog, which is the easiest 
# to conceptualize: it gives an estimate between 0 and 1 of probability of presence. 

