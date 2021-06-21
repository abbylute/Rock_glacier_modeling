# Calculate AICs across model complexities
# run on marvin


dyn.load("/opt/modules/climatology/gdal/3.0.2/lib/libgdal.so")


library(rJava)
library(tictoc)
library(sf) #library(sf,lib='/mnt/ceph/alute/Rpackages/') 
library(tidyverse)
library(blockCV)
library(parallel)
library(ENMeval) # vignette: https://cran.r-project.org/web/packages/ENMeval/vignettes/ENMeval-vignette.html


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

# add rain
samp$rain <- samp$ppt-samp$sfe
bgr$rain <- bgr$ppt-bgr$sfe


# create groups for cross validation
#---------------------------------------------

# environmental blocks:
s2 <- samp %>% mutate(fold = if_else(tmean>median(tmean),1,2))
bgfold <- bgr %>% mutate(fold = if_else(tmean>median(samp$tmean),1,2))
bgfold <- as.numeric(bgfold$fold)

# will this require too much extrapolation? Could do user-defined groups instead.
blocks <- readRDS('WUS/Data/CV/spatial_blocks_bs400000.RData')
bl <- blocks$blocks

bl <- st_as_sf(bl)


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


# remove collinear variables
#--------------------------------------------
not_use <- c('hw3','tmin','tmax','maxswe','ppt','tschange','duration')
samp <- samp %>% dplyr::select(-not_use)
bgr <- bgr %>% dplyr::select(-not_use)


occ <- as.data.frame(samp[,2:dim(samp)[2]])
names(occ)[1:2] <- c("LON","LAT")
bg.coords = as.data.frame(bgr[,2:dim(bgr)[2]])
names(bg.coords)[1:2] <- c("LON","LAT")


dir <- 'WUS/Data/Masked_rasters/'
rr <- stack(paste0(dir,'maxent_variable_stack_longlat.tif'));
names(rr) <- c('aspect','slope','hw5','hw3','tmin','tmax','tmean','tschange','ppt',
               'sw','sfe','maxswe','duration','nosnowdays','lith')
rr[[16]]  <- rr[[9]]-rr[[11]]
names(rr)[16] <- 'rain'
rr <- subset(rr, c(1:3,7,10:11,14:16))


betas = c(1,3,5,7,9)
feats = c('L','LQ','LT','LH','LQH','LQT','LQTH')
outdf = data.frame('beta'=rep(betas,length(feats)), 'feat'=rep(feats,each=length(betas)),'AICc'=NA)
categoricals = 'lith'
userArgs <- NULL
dismo.vs <- packageVersion("dismo")
v <- maxentJARversion()
alg <- paste("Maxent", v, "via dismo", dismo.vs)

pres <- occ %>% dplyr::select(names(rr))
bg <- bg.coords %>% dplyr::select(names(rr))
for (i in 1:length(categoricals)) {
  pres[, categoricals[i]] <- as.factor(pres[, categoricals[i]])
  bg[, categoricals[i]] <- as.factor(bg[, categoricals[i]])
}
x <- rbind(pres, bg)
p <- c(rep(1, nrow(pres)), rep(0, nrow(bg)))
pred.args <- c("outputformat=raw", "doclamp=true")

for (ii in 1:nrow(outdf)){
  print(ii)
  tic()
  RMvalues = outdf$beta[ii]
  fc = outdf$feat[ii]

  args <- make.args(RMvalues, fc)
  args.lab <- make.args(RMvalues, fc, labels = TRUE)
  # tuning script:
  # modelTune
  full.mod <- dismo::maxent(x, p, args = c(args[[1]], userArgs), 
                            factors = categoricals)
  predictive.map <- predict(full.mod, rr, args = pred.args)
  nparam <- get.params(full.mod)
  aicc <- calc.aicc(nparam, occ[,1:2], predictive.map)
  outdf$AICc[ii] <- aicc$AICc
  print(aicc$AICc)
  
  toc()
}

# save outdf
write.table(outdf,paste0(outdir,'aic_table.txt'))


