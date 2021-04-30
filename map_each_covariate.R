# Make map of each covariate

#dyn.load("/opt/modules/climatology/gdal/3.0.2/lib/libgdal.so")

library(raster)
library(tidyverse)
library(RColorBrewer)
library(grid) # gpar
library(pals)

outdir <- 'WUS/Figures/'

dir <- 'WUS/Data/Masked_rasters/'
rr <- stack(paste0(dir,'maxent_variable_stack_longlat.tif'));
names(rr) <- c('aspect','slope','hw5','hw3','tmin','tmax','tmean','tschange','ppt',
               'swdown','sfe','maxswe','duration','nosnowdays','lith')
rr <- subset(rr, c(1:3,7:8,10:11,13:15))

xy <- coordinates(rr[[1]])

nms <- c('Aspect (°)', 'Slope (°)', 'Headwall', 'Mean Annual\nTemperature\n(°C)',
         'Freeze-Thaw\nCycles', expression('Downward\nShortwave\n(Wm'^-2*')      '), 
         'Snowfall Water\nEquivalent (mm)', 'Snow\nDuration\n(days)', 
         'Snow free\n days (days)')

cbs <- c('Spectral', # aspect
         'Spectral', # slope
         'Spectral', # headwall
         'RdYlBu', # tmean
         'RdYlBu', # tschange
         'RdYlBu', # solar
         'YlGnBu', # SFE
         'YlGnBu', # snow duration
         'RdYlBu') # nosnowdays

varmod = c(1:5,8:9)
for (ll in seq_along(varmod)){
  outnm <- names(rr)[ll]
  
  df1 <- as.data.frame(cbind(xy,values(rr[[ll]])))
  names(df1) <- c('x','y','val')
  df1 <- df1 %>% filter(!is.na(val))
  
  if (ll==1){
    colo <- kovesi.cyclic_mygbm_30_95_c78_s25(100)
      #kovesi.cyclic_mygbm_30_95_c78
    #kovesi.cyclic_mygbm_30_95_c78_s25
  }else{
    colo <- rev(brewer.pal(11,cbs[ll]))
  }
  
  g1 <- ggplot() + 
    borders('state', fill='grey95') +
    geom_tile(data=df1, aes(x=x,y=y,color=val)) +
    #borders('world') +
    #geom_point(data=rgs, aes(x=lon,y=lat), size=.6) +
    coord_cartesian(x=c(-124.5,-104.5), y = c(31.5,49)) + 
    scale_color_gradientn(colors=colo, name = nms[ll]) + # trans='log'
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = c(0.1, 0.14),
          legend.title = element_text(size = 8)) +
    labs(x=NULL,y=NULL) 
  jpeg(paste0(outdir,outnm,'_map.jpeg'), units = 'in', width = 6, height = 6, res = 500)
  g1
  dev.off()
}

#may need to do these separately
# nosnowdays (color scale)
# lith (categorical)
####


# shortwave (separately because legend label is tricky)
#-------------------------------------------------------
ll=6;
outnm <- names(rr)[ll]

df1 <- as.data.frame(cbind(xy,values(rr[[ll]])))
names(df1) <- c('x','y','val')
df1 <- df1 %>% filter(!is.na(val))

ln1 <- expression('Downward')
ln2 <- expression('Shortwave')
ln3 <- expression('(Wm'^-2*')')
g1 <-  ggplot() + 
  borders('state', fill='grey95') +
  geom_tile(data=df1, aes(x=x,y=y,color=val)) +
  coord_cartesian(x=c(-124.5,-104.5), y = c(31.5,49)) + 
  scale_color_gradientn(colors=rev(brewer.pal(11,cbs[ll])),name=NULL) + #, name = nm6) + # trans='log'
  annotation_custom(grid::textGrob(ln1, gp = gpar(fontsize=8)), xmin = -124, xmax = -124, ymin = 37, ymax = 37) +
  annotation_custom(grid::textGrob(ln2, gp = gpar(fontsize=8)), xmin = -124, xmax = -124, ymin = 36.5, ymax = 36.5) +
  annotation_custom(grid::textGrob(ln3, gp = gpar(fontsize=8)), xmin = -124, xmax = -124, ymin = 36, ymax = 36) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.08, 0.14),
        legend.title = element_text(size = 8)) +
  labs(x=NULL,y=NULL) 
jpeg(paste0(outdir,outnm,'_map.jpeg'), units = 'in', width = 6, height = 6, res = 500)
g1
dev.off()


# SFE (separately for different color scale)
#----------------------------------------------
ll=7
outnm <- names(rr)[ll]

df1 <- as.data.frame(cbind(xy,values(rr[[ll]])))
names(df1) <- c('x','y','val')
df1 <- df1 %>% filter(!is.na(val))

g1 <- ggplot() + 
  borders('state', fill='grey95') +
  geom_tile(data=df1, aes(x=x,y=y,color=val)) +
  coord_cartesian(x=c(-124.5,-104.5), y = c(31.5,49)) + 
  scale_color_gradientn(colors=rev(brewer.pal(11,cbs[ll])), name = nms[ll], trans='log',
                        breaks = c(50,100,300,500,1000,3000,5000)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.1, 0.14),
        legend.title = element_text(size = 8)) +
  labs(x=NULL,y=NULL) 
jpeg(paste0(outdir,outnm,'_map.jpeg'), units = 'in', width = 6, height = 6, res = 500)
g1
dev.off()


# No snow days (separately for different color scale)
#----------------------------------------------
ll=9
outnm <- names(rr)[ll]

df1 <- as.data.frame(cbind(xy,values(rr[[ll]])))
names(df1) <- c('x','y','val')
df1 <- df1 %>% filter(!is.na(val))

g1 <- ggplot() + 
  borders('state', fill='grey95') +
  geom_tile(data=df1, aes(x=x,y=y,color=val)) +
  coord_cartesian(x=c(-124.5,-104.5), y = c(31.5,49)) + 
  scale_color_gradientn(colors=rev(brewer.pal(11,cbs[ll])), name = nms[ll], trans='log',
                        breaks = c(0,10,30,60,120)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.1, 0.14),
        legend.title = element_text(size = 8)) +
  labs(x=NULL,y=NULL) 
jpeg(paste0(outdir,outnm,'_map.jpeg'), units = 'in', width = 6, height = 6, res = 500)
g1
dev.off()


# Lithology (separately because categorical)
#----------------------------------------------
ll <- 10
outnm <- names(rr)[ll]
licb <- 'Paired'

df1 <- as.data.frame(cbind(xy,values(rr[[ll]])))
names(df1) <- c('x','y','val')
df1 <- df1 %>% filter(!is.na(val))
df1$val <- as.character(df1$val)

g1 <- 
  ggplot() + 
  borders('state', fill='grey95') +
  geom_tile(data=df1, aes(x=x,y=y,fill=val)) +
  coord_cartesian(x=c(-124.5,-104.5), y = c(31.5,49)) + 
  scale_fill_manual(values=rev(brewer.pal(11,licb)), name = 'Lithology\nClass',
                    breaks = c("1","2","3","4","5","6","7","8","9","10","11")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.title = element_text(size = 8)) +
  labs(x=NULL,y=NULL) 
jpeg(paste0(outdir,outnm,'_map.jpeg'), units = 'in', width = 6.7, height = 6, res = 500)
g1
dev.off()




#library(rasterVis)
#library(spData) # us_states

#data(us_states)

#cbs <- c('Spectral', # aspect
#         'Spectral') 

#for (ll in 1:dim(rr)[3]){
#  nm <- names(rr)[ll]
#  cb <- cbs(ll)
#  p1 <- gplot(rr[[ll]], maxpixels= 100000000) +
#    geom_tile(aes(fill=value)) +
#    scale_fill_gradientn(colors=brewer.pal(11,cb), na.value = 'transparent')+
#    borders('state', colour='grey30', size=.5) +
#    coord_quickmap(xlim=c(-125,-104), ylim=c(32,49)) +
#    theme_bw() +
#    theme(panel.grid = element_blank()) +
#    labs(x=NULL,y=NULL,fill=nm)
  
#  jpeg(paste0(outdir,nm,'_map.jpeg'), units = 'in', width = 6, height = 6, res = 500)
#  p1
#  dev.off()
#}

#xy <- coordinates(rr[[1]])

#df1 <- as.data.frame(cbind(xy,values(rr[[1]])))
#names(df1) <- c('x','y','val')


#g1 <- ggplot() + 
#  borders('state', fill='grey95') +
#  geom_tile(data=df1, aes(x=x,y=y,color=val)) +
  #borders('world') +
  #geom_point(data=rgs, aes(x=lon,y=lat), size=.6) +
#  coord_cartesian(x=c(-124.5,-104.5), y = c(31.5,49)) + 
#  scale_color_gradientn(colors=terrain.colors(30), name = 'Aspect (°)') +
#  theme_bw() +
#  theme(panel.grid = element_blank(),
#        legend.position = c(0.1, 0.14)) +
#  labs(x=NULL,y=NULL)
#jpeg(paste0(outdir,'aspect2_map.jpeg'), units = 'in', width = 6, height = 6, res = 500)
#g1
#dev.off()

 
#us_states_sp <- as(us_states, Class='Spatial')

#levelplot(rr[[1]],
#          maxpixels = 10000,
#          margin = FALSE,
#          xlim = c(-125,-104),
#          ylim = c(32,49.5)) +
#  layer((us_states_sp))

#p + title('aspect')

#xy <- coordinates(rr)

#df1 <- as.data.frame(cbind(xy,values(rr[[1]])))
#names(df1) <- c('x','y','val')

#ggplot(df1) +
#  geom_raster(aes(x=x,y=y,fill=val))
