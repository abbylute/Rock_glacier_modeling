# How does rock glacier suitability change at WUS glacier locations?
# Do glacier areas become more suitable for rock glaciers?

library(raster)
library(tidyverse)
library(sf)
library(fasterize)

thres <- .212

# 1. Create raster of locations that enhanced between pre and ctrl
dir1 <- 'WUS/Data/Maxent_outputs/May-26-2021/'
pred_pre <- raster(paste0(dir1,'preindustrial_predictions.tif'))
pred_ctrl <- raster(paste0(dir1,'ctrl_predictions.tif'))
pred_pgw <- raster(paste0(dir1,'pgw_predictions.tif'))

# 2. load glacier locations
glims <- st_read('/Volumes/WDPassport/DATA/GLIMS/glims_polygons.shp')
glimsr <- fasterize(glims, raster=pred_pre, fun='last')
gdf <- data.frame(coordinates(glimsr), values(glimsr))
names(gdf) <- c('x','y','glimsTF')
gdf <- gdf %>% filter(!is.na(glimsTF))

# 3. extract predictions for glacier locations
gpre <- raster::extract(pred_pre, gdf[,1:2])
gctrl <- raster::extract(pred_ctrl, gdf[,1:2])
gpgw <- raster::extract(pred_pgw, gdf[,1:2])

gdf <- cbind(gdf,gpre,gctrl,gpgw)
gdf <- gdf %>% filter(!is.na(gpre)) # remove 4 cells that were not modeled

mean(gdf$gpre)
mean(gdf$gctrl)
mean(gdf$gpgw)

# on net, glaciers become less suitable with each step forward in time
gdf %>% pivot_longer(cols=gpre:gpgw) %>% group_by(name) %>% summarize(sum(value>thres)/n())

gdf %>% pivot_longer(cols=gpre:gpgw) %>%
ggplot() +
  geom_density(aes(x=value,group=name,color=name))

# however, there are some glaciers that stay suitable or become more suitable:
ggplot(gdf) +
  geom_point(aes(x=gpre,y=gctrl))

# they are scatter across the WUS, but the ones with the highest ctrl suitability are in
# the sierras and wyoming
gdf %>% filter(gctrl>thres) %>%
  ggplot() +# borders('state') + #coord_cartesian(xlim=c(-125,-119),ylim=c(46,49))+
  geom_point(aes(x=x,y=y,color=gctrl))

# in the future scenario, only glaciers in ca and wy will remain suitable, with generally low
# suitability values
gdf %>% filter(gpgw>thres) %>%
  ggplot() +
  geom_point(aes(x=x,y=y,color=gpgw))

# percent of glaciated grid cells that have enhanced suitabiltiy between pre and ctrl:
gdf %>% mutate(en = gpre<thres & gctrl>thres) %>% summarize(sum(en)/n()*100)

# enhanced locations are almost exclusively in the middle rockies
gdf %>% filter(gpre<thres & gctrl>thres) %>%
  ggplot() +
  geom_point(aes(x=x,y=y,color=gctrl)) +
  lims(x=c(-125,-104),y=c(30,50))



# What distinguishes glaciers which may become rock glaciers from those that don't?
#-----------------------------------------------------------------------------------
gdf <- gdf %>% 
  mutate(delt12 = case_when(gpre>thres & gctrl>thres ~ 'persist',
                            gpre<thres & gctrl<thres ~ 'never suitable',
                            gpre>thres & gctrl<thres ~ 'disappear',
                            gpre<thres & gctrl>thres ~ 'enhance'),
         delt23 = case_when(gctrl>thres & gpgw>thres ~ 'persist',
                            gctrl<thres & gpgw<thres ~ 'never suitable',
                            gctrl>thres & gpgw<thres ~ 'disappear',
                            gctrl<thres & gpgw>thres ~ 'enhance'))
gdf$x <- round(gdf$x,4)
gdf$y <- round(gdf$y,4)

elev <- raster('/Volumes/WDPassport/DATA/DEM/NED/new/WUS_NED_210m.tif')

var_remove <- c('duration','hw3','maxswe','ppt','tmax','tmin','tschange')
dfpre <- read_csv('WUS/Data/Maxent_tables/background_PRE.txt')
dfpre$lon <- round(dfpre$lon,4)
dfpre$lat <- round(dfpre$lat,4)
dfpre <- left_join(gdf, dfpre, by = c('x'='lon','y'='lat'))
dfpre$rain <- dfpre$ppt - dfpre$sfe
dfpre <- dfpre %>% dplyr::select(-all_of(var_remove),-c(glimsTF,gpre,gctrl,gpgw,Id))
dfpre$elevation <- raster::extract(elev, dfpre[,1:2]) 
dfpre <- dfpre %>% mutate(per='preindustrial') %>%
  pivot_longer(cols=aspect:elevation, names_to='var', values_to='val')

dfctrl <- read_csv('WUS/Data/Maxent_tables/background_CTRL.txt')
dfctrl$lon <- round(dfctrl$lon,4)
dfctrl$lat <- round(dfctrl$lat,4)
dfctrl <- left_join(gdf, dfctrl, by = c('x'='lon','y'='lat'))
dfctrl$rain <- dfctrl$ppt - dfctrl$sfe
dfctrl <- dfctrl %>% dplyr::select(-all_of(var_remove),-c(glimsTF,gpre,gctrl,gpgw,Id))
dfctrl$elevation <- raster::extract(elev, dfctrl[,1:2]) 
dfctrl <- dfctrl %>% mutate(per='present') %>%
  pivot_longer(cols=aspect:elevation, names_to='var', values_to='val')

dfpgw <- read_csv('WUS/Data/Maxent_tables/background_PGW.txt')
dfpgw$lon <- round(dfpgw$lon,4)
dfpgw$lat <- round(dfpgw$lat,4)
dfpgw <- left_join(gdf, dfpgw, by = c('x'='lon','y'='lat'))
dfpgw$rain <- dfpgw$ppt - dfpgw$sfe
dfpgw <- dfpgw %>% dplyr::select(-all_of(var_remove),-c(glimsTF,gpre,gctrl,gpgw,Id))
dfpgw$elevation <- raster::extract(elev, dfpgw[,1:2]) 
dfpgw <- dfpgw %>% mutate(per='future') %>%
  pivot_longer(cols=aspect:elevation, names_to='var', values_to='val')


tab <- rbind(dfpre, dfctrl, dfpgw)


tab$delt12 <- factor(tab$delt12, levels = c('never suitable','disappear','persist','enhance'))
tab$delt23 <- factor(tab$delt23, levels = c('never suitable','disappear','persist','enhance'))
tab$per <- factor(tab$per, levels = c('preindustrial','present','future'))
tab$var <- factor(tab$var, levels = c('tmean','rain','sfe','nosnowdays','sw','aspect','slope','hw5','elevation','lith'))
lbs = setNames(c("'tmean ' (degree*C)", 
                 "'rain (mm)'",
                 "'sfe (mm)'",
                 "nosnowdays",
                 "'solar (W '*m^-2*')'",
                 "'aspect ' (degree)", 
                 "'slope ' (degree)", 
                 "headwall5",
                 "'elevation (m)'",
                 "rocktype"),
               c('tmean','rain','sfe','nosnowdays','sw','aspect','slope','hw5','elevation','lith'))[levels(tab$var)]

tab12 <- tab %>% filter(per %in% c('preindustrial','present'))
notes <- tab12 %>% group_by(delt12) %>% summarize('perc'=round(n()/nrow(tab12)*100,1)); notes
notes$perc <- paste0(as.character(notes$perc),'%')
notes$var <- factor('tmean')
  
g1 <- ggplot(tab12) +
  geom_violin(data = subset(tab12, var %in% c('tmean','rain','sfe','nosnowdays','sw')),
              aes(x=delt12, y=val, color=per)) +
  geom_violin(data = subset(tab12, var %in% c('aspect','slope','hw5','elevation','lith')),
              aes(x=delt12, y=val)) +
  geom_text(data = notes, aes(x=delt12,y=9,label=perc),size=1.8) +
  scale_color_manual(values=c('blue','purple')) +
  facet_wrap(~var, scales='free_y',
             labeller = as_labeller(lbs, label_parsed),
             nrow=2) +
  labs(x=NULL, y = 'covariate value', color=NULL) +
  theme_bw() +
  theme(strip.background=element_blank(),
        strip.text = element_text(size=7),
        axis.text.x = element_text(hjust=1, angle=40, size=5),
        axis.text.y = element_text(size=5),
        axis.title.y = element_text(size=7),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=7))
jpeg('WUS/Figures/covariate_distributions_glaciers_pre_to_present.jpeg',units='in',width=8,height=4,res=400)
g1
dev.off()

tab23 <- tab %>% filter(per %in% c('present','future'))
notes <- tab23 %>% group_by(delt23) %>% summarize('perc'=round(n()/nrow(tab23)*100,1)); notes
notes$perc <- paste0(as.character(notes$perc),'%')
notes <- notes %>% add_row(delt23='enhance',perc='0%')
notes$var <- factor('tmean')
notes$delt23 <- factor(notes$delt23, levels = levels(tab23$delt23))

g2 <- ggplot(tab23) +
  geom_violin(data = subset(tab23, var %in% c('tmean','rain','sfe','nosnowdays','sw')),
              aes(x=delt23, y=val, color=per)) +
  geom_violin(data = subset(tab23, var %in% c('aspect','slope','hw5','elevation','lith')),
              aes(x=delt23, y=val)) +
  geom_text(data = notes, aes(x=delt23,y=13,label=perc),size=1.8) +
  scale_color_manual(values=c('purple','red')) +
  scale_x_discrete(limits = c('never suitable','disappear','persist','enhance')) +
  facet_wrap(~var, scales='free_y',
             labeller = as_labeller(lbs, label_parsed),
             nrow=2) +
  labs(x=NULL, y = 'covariate value', color=NULL) +
  theme_bw() +
  theme(strip.background=element_blank(),
        strip.text = element_text(size=7),
        axis.text.x = element_text(hjust=1, angle=40, size=5),
        axis.text.y = element_text(size=5),
        axis.title.y = element_text(size=7),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=7))
jpeg('WUS/Figures/covariate_distributions_glaciers_present_to_pgw.jpeg',units='in',width=8,height=4,res=400)
g2
dev.off()

