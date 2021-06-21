
library(tidyverse)
library(raster)
library(sf)
library(gridExtra)
library(khroma)

dir <- 'WUS/Data/Maxent_outputs/May-26-2021/'

# load RG locations
samp <- read_csv('WUS/Data/Maxent_tables/sample.txt')

# load IG locations
#ig <- read_csv('/Users/abbylute/Documents/IGERT/Glaciers/RGI_data/02_rgi60_WesternCanadaUS.csv')
#ig <- ig %>% dplyr::select(CenLon, CenLat) %>% filter(CenLat < 50)
#names(ig) <- c('lon','lat')

# load glacier outlines
glims <- st_read('/Users/abbylute/Downloads/glims_download_82381/glims_polygons.shp')
#glims <- st_crop(glims, xmin = -123, xmax = -120, ymin = 47, ymax = 49)


# prepare prediction data
pre <- raster(paste0(dir,'preindustrial_predictions.tif'));
pgw <- raster(paste0(dir,'pgw_predictions.tif'));
xy <- coordinates(pre)


# look at the distribution of future probabilities at rg locations
#samp_pgw <- raster::extract(pgw, samp[,2:3])
#samp_pre <- raster::extract(pre, samp[,2:3])
#ig_pgw <- raster::extract(pgw, ig)
#ig_pre <- raster::extract(pre, ig)
#hist(samp_pgw, breaks=20)


# create three value raster
# persist: values greater than .7 in pre and pgw
# disappear: values greater than .7 in pre, and less than .3 in pgw
persist <- .212 #(based on maxent.html table and Liu et al., 2013 article on threshold selection)
disappear <- .212
rast3 <- pgw
values(rast3) <- NA
rast3[pre > persist & pgw > persist] <- 1
rast3[pre > persist & pgw < disappear] <- 0
rast3[pre < disappear & pgw > persist] <- 2
rast3[pre < disappear & pgw < disappear] <- 3

rast3df <- as.data.frame(cbind(xy, values(rast3)))
names(rast3df) <- c('x','y','val')
rast3df <- rast3df %>% filter(!is.na(val))
rast3df$val <- as.character(rast3df$val)
rast3df$name <- recode(rast3df$val, `1`='persist', `0`='disappear', `2`='enhance', `3`='never suitable')
rast3df$name <- factor(rast3df$name, levels = c('never suitable','disappear','persist','enhance'))

#val2name <- data.frame('val'=c(0,1,2,3),'name'=c('persist','disappear','enhance','never suitable'))

quartz()
# Map across WUS:
p1 <- ggplot() + 
  borders('state', fill = 'grey95') +
  geom_tile(data = rast3df, aes(x = x, y = y, fill = val)) +
  coord_sf(x = c(-124.5,-104.5), y = c(31.5,48.5)) + 
  scale_fill_manual(values = c('grey50','darkorange1','purple','green'), 
                    labels = c('never suitable','disappear','persist','enhance'),
                    name = NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.09, 0.16),
        legend.title = element_text(size = 8)) +
  labs(x=NULL,y=NULL) 
p1

jpeg(paste0(dir,'prediction_case_WUS.jpeg'), units = 'in', width = 6, height = 6, res = 500)
p1
dev.off()


lt <- colour('light')
lt(9)
colos <- c("#BBBBBB","#EE8866","#77AADD",'#BBCC33')
# Plot case 1: Rock glaciers decline
ll1 <- data.frame(xmin = -113.8, xmax = -113.4, ymin = 44.35, ymax = 44.7)
minirast <- rast3df[rast3df$y>ll1$ymin & rast3df$y<ll1$ymax & rast3df$x>ll1$xmin & rast3df$x< ll1$xmax,]
minisamp <- samp %>% filter(between(lon,ll1$xmin,ll1$xmax) & between(lat,ll1$ymin,ll1$ymax))
p1 <- ggplot() + 
  borders('state', fill = 'grey95') +
  geom_tile(data = minirast, aes(x = x, y = y, fill = name)) +
  geom_point(data = minisamp, aes(x=lon, y=lat), shape = 21) +
  coord_quickmap(x = c(ll1$xmin, ll1$xmax), y = c(ll1$ymin, ll1$ymax)) + 
  scale_fill_manual(values = colos,#c('grey50','darkorange1','purple','green'),
                    drop = FALSE,
                    name = NULL) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.2, 0.18),
        axis.text = element_text(size=6),
        legend.text = element_text(size=7)) +
  guides(fill = guide_legend(override.aes=list(size=1.2))) +
  labs(x=NULL,y=NULL,tag='b') 
p1

# Plot case 2: Rock glaciers persist
#ll2 <- data.frame(xmin = -110, xmax = -108.95, ymin = 42.55, ymax = 43.5) # wind river range
ll2 <- data.frame(xmin = -118.68, xmax = -118.2, ymin = 36.5, ymax = 36.97) # southern sierra
minirast <- rast3df[rast3df$y>ll2$ymin & rast3df$y<ll2$ymax & rast3df$x>ll2$xmin & rast3df$x< ll2$xmax,]
minisamp <- samp %>% filter(between(lon,ll2$xmin,ll2$xmax) & between(lat,ll2$ymin,ll2$ymax))
p2 <- ggplot() + 
  borders('state', fill = 'grey95') +
  geom_tile(data = minirast, aes(x = x, y = y, fill = name), show.legend =F) +
  #geom_sf(data = glims) +
  geom_point(data = minisamp, aes(x=lon,y=lat), shape = 21) +
  coord_quickmap(x = c(ll2$xmin, ll2$xmax), y = c(ll2$ymin, ll2$ymax)) + 
  scale_fill_manual(values = colos, #c('grey50','darkorange1','purple','green'), 
                    drop = FALSE,
                    name = NULL) +
  scale_x_continuous(expand = c(0,0)) + #, breaks = c(-110,-109.5,-109)) +
  scale_y_continuous(expand = c(0,0)) + #, breaks = c(43,43.5)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=6),
        legend.position = c(0.09, 0.16),
        legend.title = element_text(size = 8)) +
  labs(x=NULL,y=NULL,tag='a') 
p2

# Plot case 1: Rock glaciers enhance
ll3 <- data.frame(xmin = -109.75, xmax = -109.53, ymin = 44.99, ymax = 45.18)
#ll3 <- data.frame(xmin = -122.05, xmax = -121.85, ymin = 48.59, ymax = 48.75) # south twin, wa
minirast <- rast3df[rast3df$y>ll3$ymin & rast3df$y<ll3$ymax & rast3df$x>ll3$xmin & rast3df$x< ll3$xmax,]
minisamp <- samp %>% filter(between(lon,ll3$xmin,ll3$xmax) & between(lat,ll3$ymin,ll3$ymax))
miniglims <- st_crop(glims, xmin = ll3$xmin, xmax = ll3$xmax, ymin = ll3$ymin, ymax = ll3$ymax)

# quickplot
plot(crop(rast3,extent(ll3$xmin,ll3$xmax,ll3$ymin,ll3$ymax)))
plot(miniglims,add=T)

quartz() # this plot sometimes won't plot in normal window gives grid edge error
p3 <- ggplot() + 
  borders('state', fill = 'grey95') +
  geom_tile(data = minirast, aes(x = x, y = y, fill = name), show.legend =F) +
  geom_sf(data = miniglims, color='cyan', fill='transparent', size=.3) +
  geom_point(data = minisamp, aes(x=lon,y=lat), shape = 21) +
  coord_sf(x = c(ll3$xmin, ll3$xmax), y = c(ll3$ymin, ll3$ymax)) + 
  scale_fill_manual(values = colos, #c('grey50','darkorange1','purple','green'), 
                    drop = FALSE,
                    name = NULL) +
  scale_x_continuous(expand = c(0,0), breaks = c(-109.7, -109.6), labels = c('-109.7','-109.6')) +
  scale_y_continuous(expand = c(0,0), breaks = c(45, 45.1), labels = c('45.0','45.1')) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size=6),
        legend.title = element_text(size = 8)) +
  labs(x=NULL,y=NULL,tag='c') 

jpeg(paste0(dir,'prediction_case_WUS_3plot_p3.jpeg'), units = 'in', width = 4, height = 4.2, res = 500)
p3
dev.off()




jpeg(paste0(dir,'prediction_case_WUS_3plot_May26.jpeg'), units = 'in', width = 10, height = 4.2, res = 500)
grid.arrange(p2,p1,p3, nrow=1)
dev.off()




# map these only at RG locations? or across domain?

rgtab <- samp %>% select(lon, lat) 
rgtab <- cbind(rgtab, samp_pre)  
rgtab <- cbind(rgtab, samp_pgw)  
rgtab <- rgtab %>%
  mutate(case = case_when((samp_pre>persist & samp_pgw>persist) ~ 'persist',
                          (samp_pre>persist & samp_pgw<disappear) ~ 'disappear',
                          (samp_pre<disappear & samp_pgw>persist) ~ 'enhance',
                          TRUE ~ 'unclear'))

igtab <- as.data.frame(cbind(ig, ig_pre, ig_pgw))
#names(igtab)[1:2] <- c('lon','lat')
igtab <- igtab %>%
  mutate(case = case_when((ig_pgw>persist) ~ 'persist',
                          TRUE ~ 'unclear'))

p2 <- ggplot() +
  borders('state', fill = 'grey95') +
  geom_point(data = rgtab, aes(x=lon, y=lat, color = case), shape = 21) +
  scale_color_manual(values = c('darkorange1','green','purple','grey')) +
  coord_cartesian(x = c(-124.5,-104.5), y = c(31.5,48.5)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.09, 0.16),
        legend.title = element_text(size = 8)) +
  labs(x=NULL,y=NULL)   

jpeg(paste0(dir,'prediction_case_at_RGs.jpeg'), units = 'in', width = 6, height = 6, res = 500)
p2
dev.off()


