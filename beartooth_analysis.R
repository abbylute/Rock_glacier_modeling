
library(tidyverse)
library(raster)
library(sf)
library(gridExtra)
library(khroma)
library(viridis)
library(ggnewscale)
library(scico)

dir <- 'WUS/Data/Maxent_outputs/May-26-2021/'
outdir <- 'WUS/Figures/Beartooth/'

# extent to explore:
ll3 <- data.frame(xmin = -109.7, xmax = -109.55, ymin = 45.04, ymax = 45.14)

# load RG locations
samp <- read_csv('WUS/Data/Maxent_tables/sample.txt')
samp <- samp %>% filter(lon >= ll3$xmin & lon <= ll3$xmax & lat >= ll3$ymin & lat <= ll3$ymax)
# sf objects take FOREVER to plot with ggplot, so use readOGR and geom_polygon instead
#rgpoly <- st_read('/Volumes/WDPassport/Rock_glacier_research/Old/Data/Rock_glaciers/RG_Inventory_v2/RG.shp')
#rgpoly <- st_crop(rgpoly, xmin = ll3$xmin, xmax = ll3$xmax, ymin = ll3$ymin, ymax = ll3$ymax)
#rgpoly <- rgpoly[rgpoly$UPDATED_Cl==1,] # only active rgs
library(rgdal)
rgpoly1 <- readOGR('/Volumes/WDPassport/Rock_glacier_research/Old/Data/Rock_glaciers/RG_Inventory_v2/RG.shp')
rgpoly1 <- crop(rgpoly1, extent(ll3$xmin, ll3$xmax, ll3$ymin, ll3$ymax))
rgpoly1 <- fortify(rgpoly1)

# load glacier outlines
# sf objects take FOREVER to plot with ggplot, so use readOGR and geom_polygon instead
##glims <- st_read('/Volumes/WDPassport/DATA/GLIMS/glims_polygons.shp')
##glims <- st_crop(glims, xmin = ll3$xmin, xmax = ll3$xmax, ymin = ll3$ymin, ymax = ll3$ymax)
#glims <- readOGR('/Volumes/WDPassport/DATA/GLIMS/glims_polygons.shp')
#glims <- crop(glims, extent(ll3$xmin, ll3$xmax, ll3$ymin, ll3$ymax))
#writeOGR(glims,'/Volumes/WDPassport/DATA/GLIMS/glims_polygons_beartooth.shp','glims',driver='ESRI Shapefile')
glims  <- readOGR('/Volumes/WDPassport/DATA/GLIMS/glims_polygons_beartooth.shp')
glims <- fortify(glims)

# elevation 
elev <- raster('/Volumes/WDPassport/DATA/DEM/NED/new/WUS_NED_210m.tif')
elev <- crop(elev, extent(ll3$xmin, ll3$xmax, ll3$ymin, ll3$ymax))
elev <- as.data.frame(cbind(coordinates(elev), values(elev)))
names(elev) <- c('x','y','elevation')

# hillshade
hill <- read_csv('WUS/Data/Domain/Beartooth_hillshade.csv')
hill <- hill %>% filter(x >= ll3$xmin & x <= ll3$xmax & y >= ll3$ymin & y <= ll3$ymax)

# prepare prediction data
pre <- raster(paste0(dir,'preindustrial_predictions.tif'));
ctrl <- raster(paste0(dir,'ctrl_predictions.tif'))
pgw <- raster(paste0(dir,'pgw_predictions.tif'));
pre <- crop(pre, extent(ll3$xmin, ll3$xmax, ll3$ymin, ll3$ymax))
ctrl <- crop(ctrl, extent(ll3$xmin, ll3$xmax, ll3$ymin, ll3$ymax))
pgw <- crop(pgw, extent(ll3$xmin, ll3$xmax, ll3$ymin, ll3$ymax))
preddf <- as.data.frame(coordinates(pre))
preddf$pre <- values(pre)
preddf$ctrl <- values(ctrl)
preddf$pgw <- values(pgw)
preddf$delt12 <- preddf$ctrl-preddf$pre
preddf$delt23 <- preddf$pgw- preddf$ctrl
preddf$delt13 <- preddf$pgw- preddf$pre
preddf <- preddf %>% filter(!is.na(pre))
rm(pre, ctrl, pgw);gc();

# create categories
persist <- .212 #(based on maxent.html table and Liu et al., 2013 article on threshold selection)
disappear <- .212
preddf <- preddf %>% mutate(cat1 = case_when(pre>persist & ctrl>persist ~ 1,
                                            pre>persist & ctrl<disappear ~ 0,
                                            pre<disappear & ctrl>persist ~ 2,
                                            pre<disappear & ctrl<disappear ~ 3),
                            cat2 = case_when(ctrl>persist & pgw>persist ~ 1,
                                            ctrl>persist & pgw<disappear ~ 0,
                                            ctrl<disappear & pgw>persist ~ 2,
                                            ctrl<disappear & pgw<disappear ~ 3))
preddf <- preddf %>% mutate(name1 = case_when(cat1 == 1 ~ 'persist',
                                             cat1 == 0 ~ 'disappear',
                                             cat1 == 2 ~ 'enhance',
                                             cat1 == 3 ~ 'not suitable'),
                            name2 = case_when(cat2 == 1 ~ 'persist',
                                             cat2 == 0 ~ 'disappear',
                                             cat2 == 2 ~ 'enhance',
                                             cat2 == 3 ~ 'not suitable'))
preddf$name1 <- factor(preddf$name1, levels = c('not suitable','disappear','persist','enhance'))
preddf$name2 <- factor(preddf$name2, levels = c('not suitable','disappear','persist','enhance'))



# 1. Plot DEM
quartz() # this plot sometimes won't plot in normal window gives grid edge error
pdem <- ggplot() + 
  #borders('state', fill = 'grey95') +
  geom_tile(data = hill, aes(x = x, y = y, fill = val), show.legend=F) +
  scale_fill_gradientn(colors=grey.colors(30)) +
  new_scale_fill() +
  geom_tile(data = elev, aes(x = x, y = y, fill = elevation),alpha=.5) +
  geom_polygon(data = rgpoly1, aes(x=long,y=lat,group=group), color='black', fill='transparent', size=.3) +
  geom_polygon(data = glims, aes(x=long,y=lat,group=group), color='white', fill='transparent', size=.3) +
  #geom_sf(data = glims, color='white', fill='transparent', size=.3) +
  #geom_sf(data = rgpoly, color = 'black', fill='transparent', size=.3) +
  ##geom_point(data = samp, aes(x=lon,y=lat), shape = 21) +
  #coord_sf(x = c(ll3$xmin, ll3$xmax), y = c(ll3$ymin, ll3$ymax)) + 
  coord_cartesian(x = c(ll3$xmin, ll3$xmax), y = c(ll3$ymin, ll3$ymax)) + 
  scale_fill_gradientn(name = 'Elevation (m)        ',colors = terrain.colors(30)) +
  scale_x_continuous(expand = c(0,0)) +#, breaks = c(-109.7, -109.6), labels = c('-109.7','-109.6')) +
  scale_y_continuous(expand = c(0,0)) +#, breaks = c(45, 45.1), labels = c('45.0','45.1')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=6),
        legend.title = element_text(size = 8)) +
  labs(x=NULL,y=NULL,tag='a') 
pdem
jpeg(paste0(outdir,'dem.jpeg'), units = 'in', width = 6, height = 6, res = 400)
pdem
dev.off()

# 2. Plot Pre-industrial Probability
quartz() # this plot sometimes won't plot in normal window gives grid edge error
ppre <- ggplot() + 
  geom_tile(data = hill, aes(x = x, y = y, fill = val), show.legend=F) +
  scale_fill_gradientn(colors=grey.colors(30)) +
  new_scale_fill() +
  geom_tile(data = preddf, aes(x = x, y = y, fill = pre), alpha =.5) +
  geom_polygon(data = rgpoly1, aes(x=long,y=lat,group=group), color='cyan3', fill='transparent', size=.4) +
  geom_polygon(data = glims, aes(x=long,y=lat,group=group), color='red', fill='transparent', size=.4) +
  #geom_sf(data = glims, color='violet', fill='transparent', size=.3) +
  #geom_sf(data = rgpoly, color = 'red', fill='transparent', size=.3) +
  coord_cartesian(x = c(ll3$xmin, ll3$xmax), y = c(ll3$ymin, ll3$ymax)) + 
  scale_fill_gradientn(colors = alpha(rev(scico(30,palette='devon')),alpha=.5), #rev(scico(30,palette='devon')),
                     'Pre-industrial\nSuitability', 
                     limits = c(0,1)) +
  scale_x_continuous(expand = c(0,0)) + #, breaks = c(-109.7, -109.6), labels = c('-109.7','-109.6')) +
  scale_y_continuous(expand = c(0,0)) +#, breaks = c(45, 45.1), labels = c('45.0','45.1')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=6),
        legend.title = element_text(size = 8)) +
  labs(x=NULL,y=NULL,tag='a') 
ppre
jpeg(paste0(outdir,'preindustrial.jpeg'), units = 'in', width = 6, height = 6, res = 400)
ppre
dev.off()


# 3. Plot Present Probability
quartz() # this plot sometimes won't plot in normal window gives grid edge error
pctrl <- ggplot() + 
  geom_tile(data = hill, aes(x = x, y = y, fill = val), show.legend=F) +
  scale_fill_gradientn(colors=grey.colors(30)) +
  new_scale_fill() +
  geom_tile(data = preddf, aes(x = x, y = y, fill = ctrl), alpha =.5) +
  geom_polygon(data = rgpoly1, aes(x=long,y=lat,group=group), color='cyan3', fill='transparent', size=.4) +
  geom_polygon(data = glims, aes(x=long,y=lat,group=group), color='red', fill='transparent', size=.4) +
  #geom_sf(data = glims, color='violet', fill='transparent', size=.3) +
  #geom_sf(data = rgpoly, color = 'red', fill='transparent', size=.3) +
  coord_cartesian(x = c(ll3$xmin, ll3$xmax), y = c(ll3$ymin, ll3$ymax)) + 
  scale_fill_gradientn(colors = alpha(rev(scico(30,palette='devon')),alpha=.5), #rev(scico(30,palette='devon')),
                       'Present\nSuitability', 
                       limits = c(0,1)) +
  scale_x_continuous(expand = c(0,0)) + #, breaks = c(-109.7, -109.6), labels = c('-109.7','-109.6')) +
  scale_y_continuous(expand = c(0,0)) +#, breaks = c(45, 45.1), labels = c('45.0','45.1')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=6),
        legend.title = element_text(size = 8)) +
  labs(x=NULL,y=NULL,tag='b') 
pctrl
jpeg(paste0(outdir,'present.jpeg'), units = 'in', width = 6, height = 6, res = 400)
pctrl
dev.off()


# 4. Plot Future Probability
quartz() # this plot sometimes won't plot in normal window gives grid edge error
ppgw <- ggplot() + 
  geom_tile(data = hill, aes(x = x, y = y, fill = val), show.legend=F) +
  scale_fill_gradientn(colors=grey.colors(30)) +
  new_scale_fill() +
  geom_tile(data = preddf, aes(x = x, y = y, fill = pgw), alpha =.5) +
  geom_polygon(data = rgpoly1, aes(x=long,y=lat,group=group), color='cyan3', fill='transparent', size=.4) +
  geom_polygon(data = glims, aes(x=long,y=lat,group=group), color='red', fill='transparent', size=.4) +
  #geom_sf(data = glims, color='violet', fill='transparent', size=.3) +
  #geom_sf(data = rgpoly, color = 'red', fill='transparent', size=.3) +
  coord_cartesian(x = c(ll3$xmin, ll3$xmax), y = c(ll3$ymin, ll3$ymax)) + 
  scale_fill_gradientn(colors = alpha(rev(scico(30,palette='devon')),alpha=.5), #rev(scico(30,palette='devon')),
                       'Future\nSuitability', 
                       limits = c(0,1)) +
  scale_x_continuous(expand = c(0,0)) + #, breaks = c(-109.7, -109.6), labels = c('-109.7','-109.6')) +
  scale_y_continuous(expand = c(0,0)) +#, breaks = c(45, 45.1), labels = c('45.0','45.1')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=6),
        legend.title = element_text(size = 8)) +
  labs(x=NULL,y=NULL,tag='c') 
ppgw
jpeg(paste0(outdir,'future.jpeg'), units = 'in', width = 6, height = 6, res = 400)
ppgw
dev.off()


# 3 panel of predictions across time
jpeg(paste0(outdir,'beartooth_continuous_prediction_3plot.jpeg'), units = 'in', width = 5, height = 11, res = 500)
grid.arrange(ppre, pctrl, ppgw, nrow=3)
dev.off()


 
# 5. Plot Categorical changes from pre to ctrl
quartz() # this plot sometimes won't plot in normal window gives grid edge error
pcat1 <- ggplot() + 
  #borders('state', fill = 'grey95') +
  geom_tile(data = hill, aes(x = x, y = y, fill = val), show.legend=F) +
  scale_fill_gradientn(colors=grey.colors(30)) +
  new_scale_fill() +
  geom_tile(data = preddf, aes(x = x, y = y, fill = name1), alpha = .5) +
  #geom_sf(data = glims, color='white', fill='transparent', size=.3) +
  #geom_sf(data = rgpoly, color = 'black', fill='transparent', size=.3) +
  geom_polygon(data = rgpoly1, aes(x=long,y=lat,group=group), color='black', fill='transparent', size=.3) +
  geom_polygon(data = glims, aes(x=long,y=lat,group=group), color='white', fill='transparent', size=.3) +
  coord_cartesian(x = c(ll3$xmin, ll3$xmax), y = c(ll3$ymin, ll3$ymax)) + 
  #geom_point(data = samp, aes(x=lon,y=lat), shape = 21) +
  #coord_sf(x = c(ll3$xmin, ll3$xmax), y = c(ll3$ymin, ll3$ymax)) + 
  scale_fill_manual(name = 'Change in\nsuitability', 
                    values = c('grey50','darkorange1','lightblue','blue')) +
  scale_x_continuous(expand = c(0,0)) +#, breaks = c(-109.7, -109.6), labels = c('-109.7','-109.6')) +
  scale_y_continuous(expand = c(0,0)) +#, breaks = c(45, 45.1), labels = c('45.0','45.1')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=6),
        legend.title = element_text(size = 8)) +
  labs(x=NULL,y=NULL,tag='b') 
pcat1
jpeg(paste0(outdir,'categorical_change_pre_to_ctrl.jpeg'), units = 'in', width = 6, height = 6, res = 400)
pcat1
dev.off()


# 6. Plot Categorical changes from pre to pgw
quartz() # this plot sometimes won't plot in normal window gives grid edge error
pcat2 <- ggplot() + 
  #borders('state', fill = 'grey95') +
  geom_tile(data = hill, aes(x = x, y = y, fill = val), show.legend=F) +
  scale_fill_gradientn(colors=grey.colors(30)) +
  new_scale_fill() +
  geom_tile(data = preddf, aes(x = x, y = y, fill = name2), alpha =.5) +
  #geom_sf(data = glims, color='white', fill='transparent', size=.3) +
  #geom_sf(data = rgpoly, color = 'black', fill='transparent', size=.3) +
  geom_polygon(data = rgpoly1, aes(x=long,y=lat,group=group), color='black', fill='transparent', size=.3) +
  geom_polygon(data = glims, aes(x=long,y=lat,group=group), color='white', fill='transparent', size=.3) +
  coord_cartesian(x = c(ll3$xmin, ll3$xmax), y = c(ll3$ymin, ll3$ymax)) + 
  #geom_point(data = samp, aes(x=lon,y=lat), shape = 21) +
  #coord_sf(x = c(ll3$xmin, ll3$xmax), y = c(ll3$ymin, ll3$ymax)) + 
  scale_fill_manual(name = 'Change in\nsuitability', 
                    drop = FALSE,
                    values = c('grey50','darkorange1','lightblue','blue')) +
  scale_x_continuous(expand = c(0,0)) +#, breaks = c(-109.7, -109.6), labels = c('-109.7','-109.6')) +
  scale_y_continuous(expand = c(0,0)) +#, breaks = c(45, 45.1), labels = c('45.0','45.1')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=6),
        legend.title = element_text(size = 8)) +
  labs(x=NULL,y=NULL,tag='c') 
pcat2
jpeg(paste0(outdir,'categorical_change_pre_to_pgw.jpeg'), units = 'in', width = 6, height = 6, res = 400)
pcat2
dev.off()
rm(pcat1,pcat2);gc();


jpeg(paste0(outdir,'beartooth_prediction_3plot.jpeg'), units = 'in', width = 5, height = 11, res = 500)
grid.arrange(pdem, pcat1, pcat2, nrow = 3)
dev.off()




##### Plot covariate changes over time for Rock glacier locations and glacier locations #####
# load sf versions of rock glacier and glacier polygons for this analysis:
rgpoly <- st_read('/Volumes/WDPassport/Rock_glacier_research/Old/Data/Rock_glaciers/RG_Inventory_v2/RG.shp')
rgpoly <- st_crop(rgpoly, xmin = ll3$xmin, xmax = ll3$xmax, ymin = ll3$ymin, ymax = ll3$ymax)
rgpoly <- rgpoly[rgpoly$UPDATED_Cl==1,] # only active rgs
glims <- st_read('/Volumes/WDPassport/DATA/GLIMS/glims_polygons.shp')
glims <- st_crop(glims, xmin = ll3$xmin, xmax = ll3$xmax, ymin = ll3$ymin, ymax = ll3$ymax)

#extract rg and ig from preddf 
predsf <- st_as_sf(preddf, coords = c('x','y'), crs = '+proj=longlat +datum=WGS84 +no_defs')
rgdf <- st_join(rgpoly, predsf)
igdf <- st_join(glims, predsf)

mean(rgdf$pre, na.rm=T)
mean(igdf$pre, na.rm=T)
mean(rgdf$ctrl, na.rm=T)
mean(igdf$ctrl, na.rm=T)
mean(rgdf$pgw, na.rm=T)
mean(igdf$pgw, na.rm=T)

#load pre-industrial covariates
pre <- stack('WUS/Data/Masked_rasters/PRE/maxent_variable_stack_longlat.tif')
pre <- crop(pre, extent(ll3$xmin, ll3$xmax, ll3$ymin, ll3$ymax))
pre[[16]] <- pre[[9]] - pre[[11]]
names(pre) <- c('aspect','slope','headwall5','hw3','tmin','tmax','tmean','tschange','ppt',
                 'solar','sfe','maxswe','duration','nosnowdays','rocktype','rain')
pre <- subset(pre, c(1:3,7,10:11,14:16))
pre <- as.data.frame(cbind(coordinates(pre), values(pre)))
pre <- cbind(pre, 'elevation'=elev$elevation)
pre <- left_join(pre, preddf)

rg_pre <- left_join(rgdf, pre)
rg_pre <- rg_pre %>% 
  dplyr::select(-c('OBJECTID':'UPDATED_Cl')) %>%
  mutate(grp = 'rock glacier')

ig_pre <- left_join(igdf, pre)
ig_pre <- ig_pre %>%
  dplyr::select(-c('line_type':'analysts')) %>%
  mutate(grp = 'glacier')

pre <- rbind(rg_pre,ig_pre)

pre <- pre %>% 
  pivot_longer(cols=aspect:elevation, names_to='varname',values_to='value') %>%
  mutate(per = 'pre-industrial')


#load historical covariates
ctrl <- stack('WUS/Data/Masked_rasters/CTRL/maxent_variable_stack_longlat.tif')
ctrl <- crop(ctrl, extent(ll3$xmin, ll3$xmax, ll3$ymin, ll3$ymax))
ctrl[[16]] <- ctrl[[9]] - ctrl[[11]]
names(ctrl) <- c('aspect','slope','headwall5','hw3','tmin','tmax','tmean','tschange','ppt',
                 'solar','sfe','maxswe','duration','nosnowdays','rocktype','rain')
ctrl <- subset(ctrl, c(1:3,7,10:11,14:16))
ctrl <- as.data.frame(cbind(coordinates(ctrl), values(ctrl)))
ctrl <- cbind(ctrl, 'elevation'=elev$elevation)
ctrl <- left_join(ctrl, preddf)

rg_ctrl <- left_join(rgdf, ctrl)
rg_ctrl <- rg_ctrl %>% 
  dplyr::select(-c('OBJECTID':'UPDATED_Cl')) %>%
  mutate(grp = 'rock glacier')

ig_ctrl <- left_join(igdf, ctrl)
ig_ctrl <- ig_ctrl %>%
  dplyr::select(-c('line_type':'analysts')) %>%
  mutate(grp = 'glacier')

ctrl <- rbind(rg_ctrl,ig_ctrl)

ctrl <- ctrl %>% 
  pivot_longer(cols=aspect:elevation, names_to='varname',values_to='value') %>%
  mutate(per = 'present')

# load future covarites
pgw <- stack('WUS/Data/Masked_rasters/PGW/maxent_variable_stack_longlat.tif')
pgw <- crop(pgw, extent(ll3$xmin, ll3$xmax, ll3$ymin, ll3$ymax))
pgw[[16]] <- pgw[[9]] - pgw[[11]]
names(pgw) <- c('aspect','slope','headwall5','hw3','tmin','tmax','tmean','tschange','ppt',
                 'solar','sfe','maxswe','duration','nosnowdays','rocktype','rain')
pgw <- subset(pgw, c(1:3,7,10:11,14:16))
pgw <- as.data.frame(cbind(coordinates(pgw), values(pgw)))
pgw <- cbind(pgw, 'elevation'=elev$elevation)
pgw <- left_join(pgw, preddf)

rg_pgw <- left_join(rgdf, pgw)
rg_pgw <- rg_pgw %>% 
  dplyr::select(-c('OBJECTID':'UPDATED_Cl')) %>%
  mutate(grp = 'rock glacier')

ig_pgw <- left_join(igdf, pgw)
ig_pgw <- ig_pgw %>%
  dplyr::select(-c('line_type':'analysts')) %>%
  mutate(grp = 'glacier')

pgw <- rbind(rg_pgw,ig_pgw)

pgw <- pgw %>% 
  pivot_longer(cols=aspect:elevation, names_to='varname',values_to='value') %>%
  mutate(per = 'future')

df <- rbind(pre, ctrl, pgw)
df$per <- factor(df$per, levels = c('pre-industrial','present','future'))

df <- df %>% filter(!varname == 'rocktype')
df$varname <- factor(df$varname, levels = 		c('tmean','rain','sfe','nosnowdays','solar','aspect','slope','headwall5','elevation'))

lbs = setNames(c("'tmean ' (degree*C)", 
                 "'rain (mm)'",
                 "'sfe (mm)'",
                 "nosnowdays",
                 "'solar (W '*m^-2*')'",
                 "'aspect ' (degree)", 
                 "'slope ' (degree)", 
                 "headwall5",
                 "'elevation (m)'"),
               c('tmean','rain','sfe','nosnowdays','solar','aspect','slope','headwall5','elevation'))[levels(df$varname)]

p1 <- ggplot(df) +
  geom_violin(data = subset(df, varname %in% c('tmean','rain','sfe','nosnowdays','solar')),
  			  aes(x=grp, y=value, color=per)) +
  geom_violin(data = subset(df, varname %in% c('aspect','slope','headwall5','elevation')),
  			  aes(x=grp, y=value)) +
  facet_wrap(~varname, 
  			 scales='free_y', 
  			 labeller = as_labeller(lbs, label_parsed),
  			 nrow=2) +
  scale_color_manual(values=c('blue','purple','red'), name=NULL) +
  theme_bw() +
  labs(x=NULL, y='covariate value') +
  theme(legend.position = c(.9,.25),
  		panel.grid.major.x = element_blank(),
  		strip.background = element_blank(),
  		axis.text.x = element_text(size=7))
p1
       
jpeg(paste0(outdir,'glacier_rg_covariate_dist_over_time.jpeg'), units = 'in', width = 8, height = 4, res = 400)
p1
dev.off()




##### Plot covariate changes over time for full Beartooth Domain #####
#load pre-industrial covariates
pre <- stack('WUS/Data/Masked_rasters/PRE/maxent_variable_stack_longlat.tif')
pre <- crop(pre, extent(ll3$xmin, ll3$xmax, ll3$ymin, ll3$ymax))
pre[[16]] <- pre[[9]] - pre[[11]]
names(pre) <- c('aspect','slope','headwall5','hw3','tmin','tmax','tmean','tschange','ppt',
                'solar','sfe','maxswe','duration','nosnowdays','rocktype','rain')
pre <- subset(pre, c(1:3,7,10:11,14:16))
pre <- as.data.frame(cbind(coordinates(pre), values(pre)))
pre <- cbind(pre, 'elevation'=elev$elevation)
pre <- left_join(pre, preddf)
pre <- pre %>% 
  pivot_longer(cols=aspect:elevation, names_to='varname',values_to='value') %>%
  mutate(per = 'pre-industrial')


#load historical covariates
ctrl <- stack('WUS/Data/Masked_rasters/CTRL/maxent_variable_stack_longlat.tif')
ctrl <- crop(ctrl, extent(ll3$xmin, ll3$xmax, ll3$ymin, ll3$ymax))
ctrl[[16]] <- ctrl[[9]] - ctrl[[11]]
names(ctrl) <- c('aspect','slope','headwall5','hw3','tmin','tmax','tmean','tschange','ppt',
                 'solar','sfe','maxswe','duration','nosnowdays','rocktype','rain')
ctrl <- subset(ctrl, c(1:3,7,10:11,14:16))
ctrl <- as.data.frame(cbind(coordinates(ctrl), values(ctrl)))
ctrl <- cbind(ctrl, 'elevation'=elev$elevation)
ctrl <- left_join(ctrl, preddf)
ctrl <- ctrl %>% 
  pivot_longer(cols=aspect:elevation, names_to='varname',values_to='value') %>%
  mutate(per = 'present')

# load future covarites
pgw <- stack('WUS/Data/Masked_rasters/PGW/maxent_variable_stack_longlat.tif')
pgw <- crop(pgw, extent(ll3$xmin, ll3$xmax, ll3$ymin, ll3$ymax))
pgw[[16]] <- pgw[[9]] - pgw[[11]]
names(pgw) <- c('aspect','slope','headwall5','hw3','tmin','tmax','tmean','tschange','ppt',
                'solar','sfe','maxswe','duration','nosnowdays','rocktype','rain')
pgw <- subset(pgw, c(1:3,7,10:11,14:16))
pgw <- as.data.frame(cbind(coordinates(pgw), values(pgw)))
pgw <- cbind(pgw, 'elevation'=elev$elevation)
pgw <- left_join(pgw, preddf)
pgw <- pgw %>% 
  pivot_longer(cols=aspect:elevation, names_to='varname',values_to='value') %>%
  mutate(per = 'future')

df <- rbind(pre, ctrl, pgw)
df$per <- factor(df$per, levels = c('pre-industrial','present','future'))

df <- df %>% filter(!varname == 'rocktype')
df$varname <- factor(df$varname, levels = 		c('tmean','rain','sfe','nosnowdays','solar','aspect','slope','headwall5','elevation'))

lbs = setNames(c("'tmean ' (degree*C)", 
                 "'rain (mm)'",
                 "'sfe (mm)'",
                 "nosnowdays",
                 "'solar (W '*m^-2*')'",
                 "'aspect ' (degree)", 
                 "'slope ' (degree)", 
                 "headwall5",
                 "'elevation (m)'"),
               c('tmean','rain','sfe','nosnowdays','solar','aspect','slope','headwall5','elevation'))[levels(df$varname)]

df12 <- df %>% filter(per %in% c('pre-industrial','present'))
p1 <- ggplot(df12) +
  geom_violin(data = subset(df12, varname %in% c('tmean','rain','sfe','nosnowdays','solar')),
              aes(x=name1, y=value, color=per)) +
  geom_violin(data = subset(df12, varname %in% c('aspect','slope','headwall5','elevation')),
              aes(x=name1, y=value)) +
  facet_wrap(~varname, 
             scales='free_y', 
             labeller = as_labeller(lbs, label_parsed),
             nrow=2) +
  scale_color_manual(values=c('blue','purple'), name=NULL) +
  theme_bw() +
  labs(x=NULL, y='covariate value') +
  theme(legend.position = c(.9,.25),
        axis.text.x = element_text(hjust=1, angle=40, size=7),
        panel.grid.major.x = element_blank(),
        strip.background = element_blank())
p1


jpeg(paste0(outdir,'covariate_distributions_bearthtooth_pre_to_present.jpeg'), units = 'in', width = 8, height = 4, res = 400)
p1
dev.off()

df23 <- df %>% filter(per %in% c('present','future'))
p1 <- ggplot(df23) +
  geom_violin(data = subset(df23, varname %in% c('tmean','rain','sfe','nosnowdays','solar')),
              aes(x=name2, y=value, color=per)) +
  geom_violin(data = subset(df23, varname %in% c('aspect','slope','headwall5','elevation')),
              aes(x=name2, y=value)) +
  facet_wrap(~varname, 
             scales='free_y', 
             labeller = as_labeller(lbs, label_parsed),
             nrow=2) +
  scale_color_manual(values=c('purple','red'), name=NULL) +
  scale_x_discrete(limits = c('not suitable','disappear','persist','enhance')) +
  theme_bw() +
  labs(x=NULL, y='covariate value') +
  theme(legend.position = c(.9,.25),
        axis.text.x = element_text(hjust=1, angle=40, size=7),
        panel.grid.major.x = element_blank(),
        strip.background = element_blank())
p1

jpeg(paste0(outdir,'covariate_distributions_bearthtooth_present_to_pgw.jpeg'), units = 'in', width = 8, height = 4, res = 400)
p1
dev.off()
