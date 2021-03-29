# Maxent Modeling of Rock Glaciers

# library(rJava)
library(sf)
library(tidyverse)
library(dismo)
library(ENMeval) # vignette: https://cran.r-project.org/web/packages/ENMeval/vignettes/ENMeval-vignette.html
source('WUS/Code/ENMevaluate_al.R')
source('WUS/Code/tuning_al.R')
#source('WUS/Code/aicc_cobos.R')
source('WUS/Code/modelTune.maxentJar_al.R')
source('WUS/Code/calc.aicc_al.R')

indir <- 'WUS/Data/Maxent_tables/'
outdir <- 'WUS/Data/Maxent_outputs/Mar-29-2021/'

samp <- read_csv(paste0(indir,'sample.txt'))
print('loading background')
bgr <- read_csv(paste0(indir,'background.txt'))

# randomly sample 10% of background
# (should these be sample randomly, or based on distance from presences as in Hijmans, 2012?)
xx <- sample(1:dim(bgr)[1], 10000, replace = F)
bgr <- bgr[xx,]

# DELETE?
# p is logical indicating which background pixels contain a presence
#p <- matrix(rep(0,10000),nrow=nrow(bgr),ncol=1)
#p[1:100] <- 1


# create groups for cross validation
#blocks <- get.block(samp[,2:3], bgr[,2:3])
#plot(bgr[,2:3], col='gray')
#points(samp[,2:3], pch=21, bg=blocks$occ.grp)
# will this require too much extrapolation? Could do user-defined groups instead.
blocks <- readRDS('WUS/Data/CV/spatial_blocks_bs400000.RData')
bl <- blocks$blocks

bl <- st_as_sf(bl)
bgsf <- st_as_sf(bgr[,2:3], coords = c('lon','lat'), crs = crs(bl))
rgsf <- st_as_sf(samp[,2:3], coords = c('lon','lat'), crs = crs(bl))

bgfold <- st_join(bl, bgsf)
rgfold <- st_join(bl, rgsf)

st_geometry(bgfold) = NULL
st_geometry(rgfold) = NULL


# Try my version:
# occ and bg.coords should be dataframes that start with "LON" and "LAT" columns
# cannot use checkboard cross validation options for now
occ <- as.data.frame(samp[,2:dim(samp)[2]])
names(occ)[1:2] <- c("LON","LAT")
bg.coords = as.data.frame(bgr[,2:dim(bgr)[2]])
names(bg.coords)[1:2] <- c("LON","LAT")
betas <- c(1)
feats <- c('L')
eval1 <- ENMevaluate_al(occ=occ, 
                        env=NULL, 
                        bg.coords=bg.coords,
                        occ.grp = rgfold$folds,
                        bg.grp = bgfold$folds,
                        rasterPreds=TRUE, 
                        method='user', #method='block', 
                        RMvalues=betas, 
                        fc=feats, 
                        clamp=TRUE,
                        categoricals='lith', 
                        algorithm='maxent.jar', 
                        #jackknife=TRUE, 
                        outdir=outdir) 

# can read in .dat files using read_csv

# currently cannot calculate niche overlap because predictions are not raster, may be possible to modify the fucntion to do this though
eval1@models # html
eval1@results # training and test AUCs, AICs

response(eval1@models[[2]]) # response curves (number is the betas/feats combo numbere)

dismo::pwdSample() # does pair-wise distance sampling as described in Hijmans, 2012
dismo::ssb() # assesses spatial sorting bias as discussed in Hijmans, 2012


# args:
# P = do response curves
# J = do jackknife
#mm <- maxent(bg, p, nbg=10000, factors = 'lith', path = outdir,
#             args = c("-P", "-J", "betamultiplier=1","writeplotdata=true"))

# how to calculate AIC?

#np <- get.params(mm)

#AIC <- calc.aicc(np, samp, predictive.maps)

