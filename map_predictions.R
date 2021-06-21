# Map RG predictions

library(raster)
library(tidyverse)
library(RColorBrewer)
library(grid) # gpar
library(scico)
library(cowplot)
library(gridExtra)
library(sf)

outdir <- 'WUS/Figures/'

ln1 = -109.7
ln2 = -109.55
lt1 = 45.04
lt2 = 45.14


# prepare preindustrial data
r0 <- raster('WUS/Data/Maxent_outputs/May-26-2021/preindustrial_predictions.tif');
xy <- coordinates(r0)
df0 <- as.data.frame(cbind(xy,values(r0)))
names(df0) <- c('x','y','val')
df0 <- df0 %>% filter(!is.na(val))
rm(r0,xy);gc();

# prepare historical data
r1 <- raster('WUS/Data/Maxent_outputs/May-26-2021/ctrl_predictions.tif');
xy <- coordinates(r1)
df1 <- as.data.frame(cbind(xy,values(r1)))
names(df1) <- c('x','y','val')
df1 <- df1 %>% filter(!is.na(val))
rm(r1,xy);gc();

# prepare future data
r2 <- raster('WUS/Data/Maxent_outputs/May-26-2021/pgw_predictions.tif');
xy <- coordinates(r2)
df2 <- as.data.frame(cbind(xy,values(r2)))
names(df2) <- c('x','y','val')
df2 <- df2 %>% filter(!is.na(val))
rm(r2,xy);gc();


# COMPUTE SUITABILITY VS AREA PLOT FIRST
# area per cell in km2
pre <- raster('WUS/Data/Maxent_outputs/May-26-2021/preindustrial_predictions_ecocrs.tif')
apc <- (res(pre)[1]*res(pre)[2])/1000/1000
rm(pre);gc();

tab <- data.frame('suit'=seq(0,1,.01), 
                  'Preindustrial'=NA, 
                  'Present'=NA,
                  'Future'=NA)

area_above_suit <- function(suit, preds, apc){
  area_above <- sum(preds > suit)*apc
}

for (ii in 1:nrow(tab)){
  tab$Preindustrial[ii] <- area_above_suit(tab$suit[ii], df0$val, apc)
  tab$Present[ii] <- area_above_suit(tab$suit[ii], df1$val, apc)
  tab$Future[ii] <- area_above_suit(tab$suit[ii], df2$val, apc)
}
names(tab) <- c('suit','Pre-industrial','Present','Future')
tab <- tab %>% pivot_longer(cols=`Pre-industrial`:Future)
tab$name <- factor(tab$name, levels = c('Pre-industrial','Present','Future'))
areaplot <- ggplot(tab) + 
  geom_line(aes(x=suit, y=value/10000, color=name)) +
  geom_vline(aes(xintercept = 0.212), color='grey30', linetype='dashed') +
  scale_color_manual(values = c('blue','purple','red')) + 
  scale_y_continuous(labels = function(x) format(x, scientific=F)) +
  coord_cartesian(xlim = c(0.1,1), ylim = c(0,6.6)) +
  theme_bw() +
  theme(legend.position=c(.6,.6),
        legend.background = element_rect(fill='transparent')) +
  labs(x = 'Suitability', 
       y = bquote('Area (10,000 '*km^2*')'), 
       color = 'Period',
       tag = 'd')
areaplot


## PREINDUSTRIAL MAPS ##

g0 <- ggplot() + 
  borders('state', fill = 'grey85') +
  geom_tile(data = df0, aes(x = x, y = y, fill = val)) +
  geom_rect(aes(xmin = ln1, xmax = ln2, ymin = lt1, ymax = lt2), color = 'black', fill='transparent') +
  coord_cartesian(x = c(-124.5,-104.5), y = c(31.5,48.5)) + 
  scale_fill_gradientn(colors = rev(scico(30,palette='devon')), #(scico(30, palette = 'berlin')), 
                       name = 'Suitability',limits=c(0,1)) + # (brewer.pal(9,'YlOrRd'))
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.11, 0.2),
        legend.key.size = unit(.44,'cm'),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8)) + #,
        #legend.background = element_rect(fill='transparent')) + 
  labs(x=NULL, y=NULL, tag = 'a') 


#df00 <- df0 %>% filter(x>ln1 & x< ln2 & y > lt1 & y<lt2)

#gin0 <- ggplot() + 
#  borders('state', fill='grey95') +
#  geom_tile(data=df00, aes(x=x,y=y,fill=val),show.legend=F) +
#  geom_sf(data = glims, color='chartreuse3', fill='transparent', size=.3) +
#  geom_sf(data = rgpoly, color = 'darkorange3', fill='transparent', size=.3) +
#  coord_sf(x = c(ln1, ln2), y = c(lt1, lt2)) + 
#  scale_fill_gradientn(colors=rev(scico(30,palette='devon')),
#                       limits = c(0,1)) +
#  scale_x_continuous(expand=c(0,0),name=NULL,
#  					 breaks=c(-109.65, -109.6, -109.55),
#  					 labels=c('-109.65','-109.60','-109.55'))+
#  scale_y_continuous(expand=c(0,0),name=NULL,
#  					 breaks=c(45.05,45.10,45.15),
#  					 labels=c('45.05','45.10','45.15')) + 
#  theme_bw() +
#  labs(tag = 'b')

#jpeg(paste0(outdir,'prediction_maps_6plot_top.jpeg'), units = 'in', width = 7.5, height = 11/3, res = 500)
#grid.arrange(g0, gin0, ncol = 2)
#dev.off()

# combine map and inset
#ggout0 <- ggdraw() +
#  draw_plot(g0) +
#  draw_plot(gin0, x=.09, y = .08, width = .23, height = .23)#x = .055*(w2/w1), y = .055, width = .24*(w2/w1), height = .24)



## PRESENT MAPS ##

g1 <- ggplot() + 
  borders('state', fill = 'grey85') +
  geom_tile(data = df1, aes(x = x, y = y, fill = val), show.legend=F) +
  geom_rect(aes(xmin = ln1, xmax = ln2, ymin = lt1, ymax = lt2), color = 'black', fill='transparent') +
  coord_cartesian(x = c(-124.5,-104.5), y = c(31.5,48.5)) + 
  scale_fill_gradientn(colors = rev(scico(30,palette='devon')),limits=c(0,1)) + 
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(x=NULL, y=NULL, tag = 'b') 


#df11 <- df1 %>% filter(x>ln1 & x< ln2 & y > lt1 & y<lt2)

#gin1 <- ggplot() + 
#  borders('state', fill='grey95') +
#  geom_tile(data=df11, aes(x=x,y=y,fill=val),show.legend=F) +
#  geom_sf(data = glims, color='chartreuse3', fill='transparent', size=.3) +
#  geom_sf(data = rgpoly, color = 'darkorange3', fill='transparent', size=.3) +
#  coord_sf(x = c(ln1, ln2), y = c(lt1, lt2)) + 
#  scale_fill_gradientn(colors=rev(scico(30,palette='devon')),
#                       limits = c(0,1)) +
#  scale_x_continuous(expand=c(0,0),name=NULL,
#  					 breaks=c(-109.65, -109.6, -109.55),
#  					 labels=c('-109.65','-109.60','-109.55'))+
#  scale_y_continuous(expand=c(0,0),name=NULL,
#  					 breaks=c(45.05,45.10,45.15),
#  					 labels=c('45.05','45.10','45.15')) + 
#  theme_bw() +
#  labs(tag = 'd')

# combine map and inset
#ggout1 <- ggdraw() +
#  draw_plot(g1) +
#  draw_plot(gin1, x=.09, y = .08, width = .23, height = .23)#x = .055*(w2/w1), y = .055, width = .24*(w2/w1), height = .24)



## FUTURE MAPS ##

g2 <- ggplot() + 
  borders('state', fill = 'grey85') +
  geom_tile(data = df2, aes(x = x, y = y, fill = val), show.legend=F) +
  geom_rect(aes(xmin = ln1, xmax = ln2, ymin = lt1, ymax = lt2), color = 'black', fill='transparent') +
  coord_cartesian(x = c(-124.5,-104.5), y = c(31.5,48.5)) + 
  scale_fill_gradientn(colors = rev(scico(30,palette='devon')), limits=c(0,1)) + 
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(x=NULL, y=NULL, tag = 'c') 

#df22 <- df2 %>% filter(x>ln1 & x< ln2 & y > lt1 & y<lt2)

#gin2 <- ggplot() + 
#  borders('state', fill='grey95') +
#  geom_tile(data=df22, aes(x=x,y=y,fill=val),show.legend=F) +
#  geom_sf(data = glims, color='chartreuse3', fill='transparent', size=.3) +
#  geom_sf(data = rgpoly, color = 'darkorange3', fill='transparent', size=.3) +
#  coord_sf(x = c(ln1, ln2), y = c(lt1, lt2)) + 
#  scale_fill_gradientn(colors=rev(scico(30,palette='devon')),
#                       limits =c(0,1)) + 
#  scale_x_continuous(expand=c(0,0),name=NULL,
#  					 breaks=c(-109.65, -109.6, -109.55),
#  					 labels=c('-109.65','-109.60','-109.55'))+
#  scale_y_continuous(expand=c(0,0),name=NULL,
#  					 breaks=c(45.05,45.10,45.15),
#  					 labels=c('45.05','45.10','45.15')) + 
#  theme_bw() +
#  labs(tag = 'f')

# combine map and inset
#ggout2 <- ggdraw() +
#  draw_plot(g2) +
#  draw_plot(gin2,  x=.09, y = .08, width = .23, height = .23)#x = .055, y = .055, width = .24, height = .24)

#jpeg(paste0(outdir,'prediction_maps_6plot.jpeg'), units = 'in', width = 7.5, height = 11, res = 500)
#grid.arrange(g0, gin0, g1, gin1, g2, gin2, ncol = 2)
#dev.off()

jpeg(paste0(outdir,'prediction_maps_4plot.jpeg'), units = 'in', width = 8, height = 8, res = 500)
grid.arrange(g0, g1, g2, areaplot, ncol = 2)
dev.off()



## COMBINE HISTORICAL AND FUTURE MAPS ##
#jpeg(paste0(outdir,'preindustrial_predictions_map_2plot.jpeg'), units = 'in', width = 10, height = 5, res = 500)
#grid.arrange(ggout1, ggout2, ncol = 2, widths = c(w1, w2))
#dev.off()

#rm(g1, g2, gin1, gin2, ggout1, ggout2);gc();


## MAP THE DIFFERENCE ##
#names(df2) <- c('x','y','pgw')
#names(df1) <- c('x','y','hist')
df1new <- df1
names(df1new) <- c('x','y','hist')
df2new <- df2
names(df2new) <- c('x','y','pgw')
df3 <- left_join(df2new,df1new)
df3$dval <- df3$pgw-df3$hist
rm(df1new, df2new); gc();

  df3 <- df0
  df3$dval <- df1$val-df0$val
g3 <- ggplot() + 
  borders('state', fill = 'grey90') +
  geom_tile(data = df3, aes(x = x, y = y, fill = dval)) +
  geom_rect(aes(xmin = ln1, xmax = ln2, ymin = lt1, ymax = lt2), color = 'black', fill='transparent') +
  coord_cartesian(x = c(-124.5,-104.5), y = c(31.5,48.5)) + 
  scale_fill_gradientn(colors = brewer.pal(11,'Spectral'), #scico(30,palette='roma'), #rev(scico(30, palette = 'devon')), 
                     name = 'Change in\nProbability\nof Presence',
                     limits = c(-1, 1)) + # (brewer.pal(9,'YlOrRd'))
  theme_bw() +
  theme(panel.grid = element_blank(),
      legend.title = element_text(size = 8)) +
  labs(x=NULL, y=NULL, tag = 'c') 

#jpeg(paste0(outdir,'delta_prediction.jpeg'), units = 'in', width = 5.5, height = 5, res = 500)
#g3
#dev.off()

df33 <- df3 %>% filter(x>ln1 & x< ln2 & y > lt1 & y<lt2)

gin3 <- ggplot() + 
  borders('state', fill='grey95') +
  geom_tile(data=df33, aes(x=x,y=y,fill=dval),show.legend=F) +
  geom_point(data=samp,aes(x=lon,y=lat), shape=21,size=3,color='black') +
  coord_cartesian(x=c(ln1,ln2), y = c(lt1,lt2)) + 
  scale_fill_gradientn(colors=brewer.pal(11,'Spectral'),
                       limits = c(-1, 1)) +#rev(scico(30,palette='devon'))) +#(brewer.pal(9,'YlOrRd')))+
  scale_x_continuous(expand=c(0,0),name=NULL)+
  scale_y_continuous(expand=c(0,0),name=NULL) + 
  theme_nothing() +
  theme(plot.background = element_rect(color='black'))


# combine map and inset
ggout3 <- ggdraw() +
  draw_plot(g3) +
  draw_plot(gin3,  x=.09, y = .08, width = .23, height = .23)#x = .055, y = .055, width = .24, height = .24)



# CREATE 3 PANEL PLOT

jpeg(paste0(outdir,'prediction_3plot.jpeg'), units = 'in', width = 4.5, height = 11, res = 500)
grid.arrange(ggout1, ggout2, ggout3, nrow=3)
dev.off()






# area classified as presence across all thresholds:
#df1 <- as.data.frame(cbind(xy,values(rr)))
#names(df1) <- c('x','y','val')
#df1 <- df1 %>% filter(!is.na(val))
#ggplot(df1) +
#  stat_ecdf(aes(x=val),geom='step')
