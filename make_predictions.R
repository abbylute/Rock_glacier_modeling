
# Select model to use
dir <- 'WUS/Data/Maxent_outputs/May-26-2021/'
eval1 <- readRDS(paste0(dir,'ENMeval_object.RData'))
mx = eval1@models[[1]]


# define output directory
outdir <- dir


## PREINDUSTRIAL
# prepare PRE raster:
dir <- 'WUS/Data/Masked_rasters/PRE/'
pre <- stack(paste0(dir,'maxent_variable_stack_longlat.tif'));
names(pre) <- c('aspect','slope','hw5','hw3','tmin','tmax','tmean','tschange','ppt',
               'sw','sfe','maxswe','duration','nosnowdays','lith')
# add rain
pre[[16]] <- pre[[9]] - pre[[11]]
names(pre)[16] <- 'rain'

# remove collinear variables
pre <- subset(pre, c(1:3,7,10:11,14:16))

# rearrange layers in raster to match model:
pre <- subset(pre, c(1:3,8,4:7,9))

# make predictions across preindustrial domain:
predfn <- paste0(outdir,'preindustrial_predictions.tif')
pp <- predict(mx, pre, ext=NULL, filename=predfn, progress='text')#, args="outputformat=cloglog")
# the default outputformat from the predict function is cloglog, which is the easiest 
# to conceptualize: it gives an estimate between 0 and 1 of probability of presence. 



## FUTURE
# prepare PGW raster:
dir <- 'WUS/Data/Masked_rasters/PGW/'
pgw <- stack(paste0(dir,'maxent_variable_stack_longlat.tif'));
names(pgw) <- c('aspect','slope','hw5','hw3','tmin','tmax','tmean','tschange','ppt',
                'sw','sfe','maxswe','duration','nosnowdays','lith')
# add rain
pgw[[16]] <- pgw[[9]] - pgw[[11]]
names(pgw)[16] <- 'rain'

# remove collinear variables
pgw <- subset(pgw, c(1:3,7,10:11,14:16))

# rearrange layers in raster to match model:
pgw <- subset(pgw, c(1:3,8,4:7,9))

# make predictions across pgw domain:
predfn <- paste0(outdir,'pgw_predictions.tif')
pp <- predict(mx, pgw, ext=NULL, filename=predfn, progress='text')#, args="outputformat=cloglog")



## HISTORICAL
# prepare CTRL raster:
dir <- 'WUS/Data/Masked_rasters/CTRL/'
ctrl <- stack(paste0(dir,'maxent_variable_stack_longlat.tif'));
names(ctrl) <- c('aspect','slope','hw5','hw3','tmin','tmax','tmean','tschange','ppt',
                'sw','sfe','maxswe','duration','nosnowdays','lith')
# add rain
ctrl[[16]] <- ctrl[[9]] - ctrl[[11]]
names(ctrl)[16] <- 'rain'

# remove collinear variables
ctrl <- subset(ctrl, c(1:3,7,10:11,14:16))

# rearrange layers in raster to match model:
ctrl <- subset(ctrl, c(1:3,8,4:7,9))

# make predictions across preindustrial domain:
predfn <- paste0(outdir,'ctrl_predictions.tif')
pp <- predict(mx, ctrl, ext=NULL, filename=predfn, progress='text')#, args="outputformat=cloglog")
# the default outputformat from the predict function is cloglog (also known as logistic), which is the easiest 
# to conceptualize: it gives an estimate between 0 and 1 of probability of presence. 
