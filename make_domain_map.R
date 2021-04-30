# Make Domain Map for WUS Rock Glacier study

library(raster)
library(ggplot2)
library(tidyverse)
library(scico)
library(gridExtra)
library(RColorBrewer)
library(ggnewscale)


# 1. Map Domain Map
# -----------------------------------------------------------

rgd <- raster('/Volumes/WDPassport/Rock_glacier_research/WUS/Data/Domain/rg_domain.tif')
dem <- raster('/Volumes/WDPassport/DATA/DEM/NED/new/WUS_NED_210m.tif')
rgs <- read_csv('/Volumes/WDPassport/Rock_glacier_research/WUS/Data/Rock_glaciers/active_rgs.csv')

# Create elevation layer
dem <- crop(dem,rgd)
values(dem)[is.na(values(rgd))] <- NA
xy <- coordinates(dem)
elev <- values(dem)
elevdf <- data.frame(xy)
elevdf$elev <- elev
elevdf <- elevdf %>% filter(!is.na(elev))
rm(dem,elev,xy,rgd);gc();

# Millar Rock Glacier locations
#mi <- read_csv('/Users/abbylute/Documents/IGERT/RockGlacierModeling/Data/Glacier/Millar_SierraNevada_RIF_Inventory/RIFDatabase_NSIDC.csv',skip =3)
#mi <- mi %>% filter(Activity == 'M') # only modern features, not relict

# Make Map
g1 <- ggplot() + 
  borders('state', fill='grey85') +
  geom_tile(data=elevdf, aes(x=x,y=y,color=elev)) +
  #borders('world') +
  geom_point(data=rgs, aes(x=lon,y=lat), size=.6) +
  #geom_point(data=mi, aes(x=Longitude,y=Latitude),size=.5,color='red') +
  coord_cartesian(x=c(-124.5,-104.5), y = c(31.5,49)) + 
  scale_color_gradientn(colors= scico(30,palette='batlow'), name = 'Elevation (m)') + #terrain.colors(30)
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.1, 0.14)) +
  labs(x=NULL,y=NULL)




# 2. Make 2d covariate distribution plots
#-------------------------------------------------------

# prepare data
indir <- 'WUS/Data/Maxent_tables/'
samp <- read_csv(paste0(indir,'sample.txt'))
print('loading background')
bgr <- read_csv(paste0(indir,'background.txt'))

samp <- samp %>% dplyr::select(aspect,slope,tmean,ppt:nosnowdays)
bgr <- bgr %>% dplyr::select(aspect,slope,tmean,ppt:nosnowdays)

samp <- samp %>% mutate('grp'='rock glaciers')
bgr <- bgr %>% mutate('grp'='background')
set.seed(17)
xx <- sample(1:dim(bgr)[1], 10000, replace = F)
bgr <- bgr[xx,]

# Make temperature/precipitation plot
demcb <- scico(30,palette='batlow')

p1 <-  ggplot() +
  stat_density_2d(data=bgr,aes(x=tmean,y=ppt,fill=stat(level)), geom='polygon',contour=T, show.legend=F) +
  scale_fill_gradient2(low='white',high=demcb[5])  + 
    new_scale('fill') +
  stat_density_2d(data=samp,aes(x=tmean,y=ppt,fill=stat(level)), geom='polygon',contour=T, show.legend=F, alpha=.4) +
  scale_fill_gradient2(low='white',high=demcb[22]) +  
    theme_bw() +
    labs(x='Mean Annual Temperature (°C)', y = 'Annual Precipitation (mm)') +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0))

# Make slope/aspect plot
# this creates funny boob artifact:
#p2 <-  ggplot() +
#    stat_density_2d(data=bgr,aes(x=aspect,y=slope,fill=stat(level)), geom='polygon',contour=T, show.legend=F) +
#    scale_fill_gradient2(low='white',high=demcb[5])  +   
#    new_scale('fill') +
#    stat_density_2d(data=samp,aes(x=aspect,y=slope,fill=stat(level)), geom='polygon',contour=T, show.legend=F, alpha=.6) +
#    scale_fill_gradient2(low='white',high=demcb[22]) +  
#    theme_bw() +
#    labs(x='Aspect (°)', y = 'Slope (°)') +
#    scale_x_continuous(expand=c(0,0)) +
#    scale_y_continuous(expand=c(0,0))
  
# this also doesn't work:
#ggplot() +
#  #stat_density_2d(data=bgr,aes(x=aspect, y=slope, fill=stat(level)), geom='polygon',contour=T, show.legend=F) +
#  #scale_fill_gradient2(low='white',high=demcb[5])  +   
#  #new_scale('fill') +
#  stat_density_2d(data=samp,aes(x=aspect, y=slope, fill=stat(level)), geom='polygon',contour=T, show.legend=F, alpha=.6) +
#  scale_fill_gradient2(low='white',high=demcb[22]) +  
#  coord_polar(theta = 'x') +
#  theme_bw()

# Instead, do this workaround:
# see second part of first question on this page:
#https://stackoverflow.com/questions/39528999/fix-interpolated-polar-contour-plot-function-to-works-with-current-r-and-possib

# prepare dataframes:
samp2 <- samp %>% mutate(aspect_rad = (aspect+90) * pi/180)
bgr2 <- bgr %>% mutate(aspect_rad = (aspect+90) * pi/180)

toCart <- data.frame(
  x = samp2$slope * cos(samp2$aspect_rad),
  y = samp2$slope * sin(samp2$aspect_rad)
)
toCart2 <- data.frame(
  x = bgr2$slope * cos(bgr2$aspect_rad),
  y = bgr2$slope * sin(bgr2$aspect_rad)
)

axisLines <-
  data.frame(
    x = 0, y = 0,
    xend = c(30,0,-30,0),
    yend = c(0,30,0,-30))

dirlabs <- 
  data.frame(xend = c(28,0,-28,0), yend = c(0,28,0,-28),
             angle = c('East','North','West','South'),
             ro = c(-90,0,90,0))

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
c1 <- circleFun(c(0,0),10*2,100)
c2 <- circleFun(c(0,0),20*2,100)
c3 <- circleFun(c(0,0),30*2,100)


p2 <- ggplot(toCart) +
  stat_density_2d(data=toCart2,aes(x=x, y=y, fill=stat(level)), geom='polygon',contour=T, show.legend=F) +
  scale_fill_gradient2(low='white',high=demcb[5])  +   
  new_scale('fill') +
  stat_density_2d(data=toCart,aes(x=x, y=y, fill=stat(level)), geom='polygon',contour=T, show.legend=F, alpha=.4) +
  scale_fill_gradient2(low='white',high=demcb[22]) +
  geom_path(data=c1,aes(x,y), color='grey80') +
  geom_path(data=c2,aes(x,y), color='grey80') +
  geom_path(data=c3,aes(x,y), color='grey80') +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(margin = margin(l=12,r=-15)),
        axis.ticks.length.y = unit(-0.15,'cm'),
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank()) +
  scale_y_continuous(name='Slope (°C)',breaks = c(10,20),expand=c(0,0)) +
  scale_x_continuous(name=' ',expand=c(0,0)) +
  coord_cartesian(xlim=c(-30,30),ylim=c(-30,30)) +
  geom_segment(data = axisLines,
               aes(x = x, y = y,
                   xend = xend,
                   yend = yend),color='grey80') +
  geom_text(data = dirlabs,
            aes(x = xend, y = yend, label = angle,srt=ro)) 


# 3. Save the combined plot:
#----------------------------------------------------------
jpeg('/Volumes/WDPassport/Rock_glacier_research/WUS/Figures/modeling_domain_plus2.jpg',units='in',width=12,height=8,res=500)
grid.arrange(g1, p1, p2, ncol=3, layout_matrix = cbind(c(1,1),c(1,1),c(2,3)))
dev.off()


