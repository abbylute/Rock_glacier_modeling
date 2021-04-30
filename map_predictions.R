# Map RG predictions

library(raster)
library(tidyverse)
library(RColorBrewer)
library(grid) # gpar
library(scico)
library(cowplot)

outdir <- 'WUS/Figures/'

rr <- raster('WUS/Data/Maxent_outputs/Apr-11-2021_bestmodel/LQPH6/preindustrial_predictions.tif');


xy <- coordinates(rr)

df1 <- as.data.frame(cbind(xy,values(rr)))
names(df1) <- c('x','y','val')
df1 <- df1 %>% filter(!is.na(val))
rm(rr,xy);gc();
#df0 <- df1 %>% filter(val<=.25)
#df1 <- df1 %>% filter(val>.25)


# define inset domain
ln1 <- -106.45#-113.7
ln2 <- -106.2#-113.35
lt1 <- 38.55#48.4
lt2 <- 38.75#48.65


g1 <- ggplot() + 
  borders('state', fill = 'grey85') +
  #geom_tile(data=df0, aes(x=x,y=y),fill='darkgrey') +
  geom_tile(data = df1, aes(x = x, y = y, fill = val)) +
  #geom_rect(data = indf, aes(xmin = x, xmax = x, ymin = y, ymax = y), color = 'black') +
  geom_rect(aes(xmin = ln1, xmax = ln2, ymin = lt1, ymax = lt2), color = 'black', fill='transparent') +
  coord_cartesian(x = c(-124.5,-104.5), y = c(31.5,48.5)) + 
  scale_fill_gradientn(colors = rev(scico(30, palette = 'devon')), 
                       name = 'Probability\nof Presence',limits=c(0,1)) + # (brewer.pal(9,'YlOrRd'))
  theme_bw() +
  theme(panel.grid = element_blank(),
        #legend.position = c(0.09, 0.16),
        legend.title = element_text(size = 8)) +
  labs(x=NULL,y=NULL) 
#jpeg(paste0(outdir,'preindustrial_predictions_map.jpeg'), units = 'in', width = 6, height = 6, res = 500)
#g1
#dev.off()




samp <- read_csv('WUS/Data/Maxent_tables/sample.txt')
#gin <-

#ln1 <- -113.65#-113.7
#ln2 <- -113.48#-113.35
#lt1 <- 44.35#48.4
#lt2 <- 44.55#48.65
#df00 <- df0 %>% filter(x>ln1 & x< ln2 & y > lt1 & y<lt2)
df11 <- df1 %>% filter(x>ln1 & x< ln2 & y > lt1 & y<lt2)
#dfone <- df11 %>% filter(val>.95)


gin <- ggplot() + 
  borders('state', fill='grey95') +
  #geom_tile(data=df00, aes(x=x,y=y),fill='darkgrey') +
  geom_tile(data=df11, aes(x=x,y=y,fill=val),show.legend=F) +
  #geom_tile(data=dfone, aes(x=x,y=y),fill='blue') +
  geom_point(data=samp,aes(x=lon,y=lat), shape=21,size=3,color='red') +
  coord_cartesian(x=c(ln1,ln2), y = c(lt1,lt2)) + 
  scale_fill_gradientn(colors=rev(scico(30,palette='devon'))) +#(brewer.pal(9,'YlOrRd')))+
  scale_x_continuous(expand=c(0,0),name=NULL)+
  scale_y_continuous(expand=c(0,0),name=NULL) + 
  theme_nothing() +
  theme(plot.background = element_rect(color='black'))
  #theme(axis.text = element_blank(), axis.ticks = element_blank(),
  #      panel.border = element_blank(), panel.background = element_rect(fill = 'transparent', colour = NA),
  #      panel.grid = element_blank())
gin

# combine map and inset
ggout <- ggdraw() +
  draw_plot(g1) +
  draw_plot(gin, x = .055, y = .055, width = .24, height = .24)

jpeg(paste0(outdir,'preindustrial_predictions_map.jpeg'), units = 'in', width = 6.5, height = 6, res = 500)
ggout
dev.off()




df1 <- as.data.frame(cbind(xy,values(rr)))
names(df1) <- c('x','y','val')
df1 <- df1 %>% filter(!is.na(val))
ggplot(df1) +
  stat_ecdf(aes(x=val),geom='step')
