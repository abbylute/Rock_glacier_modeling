# Make MESS Maps

library(tidyverse)
library(tictoc)
library(dismo)

hist <- read_csv('WUS/Data/Maxent_tables/background.txt')
hist$rain <- hist$ppt - hist$sfe
vars_to_rm <- c('Id','lon','lat','hw3','tmin','tmax','tschange','maxswe','ppt','duration')
hist <- hist %>% dplyr::select(!all_of(vars_to_rm))
hist <- as.data.frame(hist)

pgw <- stack('WUS/Data/Masked_rasters/PGW/maxent_variable_stack_longlat.tif')
names(pgw) <- c('aspect','slope','hw5','hw3','tmin','tmax','tmean','tschange','ppt',
               'sw','sfe','maxswe','duration','nosnowdays','lith')
pgw$rain <- pgw$ppt - pgw$sfe
pgw <- subset(pgw, c(1:3,7,10:11,14:16))
# rearrange to match dataframe
pgw <- subset(pgw, c(1:3,8,4:7,9))

tic() # 8:53 am june 9
mymess <- mess(pgw, hist, full = T) # took ~ 3.5 hrs
toc()

writeRaster(mymess,'WUS/Data/MESS/mess_maps_pre_to_pgw.tif')


