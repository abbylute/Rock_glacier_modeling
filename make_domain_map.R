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
  labs(x=NULL,y=NULL,tag='a')




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

color1 <- colorRampPalette(c('white',demcb[5]))
color2 <- colorRampPalette(c('white',demcb[22]))
#h1 = 2;
#h2 = 80;
bks1 = c(0.000015,0.00004,0.00008,0.00012,0.00016,.0002, 0.00025, 0.0003)
bks2 = c(0.000015,0.000025,0.000045,0.000065,0.000085,.000105, 0.00013, 0.00016, 0.0002)
p1 <-  ggplot() +
  stat_density_2d(data=bgr,aes(x=tmean,y=ppt,fill=stat(level)), geom='polygon',contour=T, show.legend=F,breaks=bks2) +
  #scale_fill_gradient2(low='white',high=demcb[5])  + 
  scale_fill_gradientn(colors = color1(20)[2:20]) +
    new_scale('fill') +
  stat_density_2d(data=samp,aes(x=tmean,y=ppt,fill=stat(level)), geom='polygon',contour=T, show.legend=F, alpha=.4,breaks=bks1) +
  #geom_point(data=samp,aes(x=tmean,y=ppt),color=demcb[22],size=.05) +
  #scale_fill_gradient2(low='white',high=demcb[22]) +  
  scale_fill_gradientn(colors = color2(20)[5:20]) +
    theme_bw() +
    labs(x='Mean Annual Temperature (°C)', y = 'Annual Precipitation (mm)', tag = 'b') +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0))
p1
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

# use this function to transform aspects from
#  - 0 at North, increasing clockwise. to
#  - 0 at East, increasing counterclockwise
# in order to match expectations for converting to radians
transform_aspect = function(a){
  b <- abs((a-90) - 360)
  b[b>360] <- b[b>360]-360
  b
}

# prepare dataframes:
samp2 <- samp %>% mutate(aspect2 = transform_aspect(aspect),
                       aspect_rad = (aspect2) * pi/180)
bgr2 <- bgr %>% mutate(aspect2 = transform_aspect(aspect),
                       aspect_rad = (aspect2) * pi/180)

toCart <- data.frame(
  x = samp2$slope * cos(samp2$aspect_rad),
  y = samp2$slope * sin(samp2$aspect_rad)
)
toCart2 <- data.frame(
  x = bgr2$slope * cos(bgr2$aspect_rad),
  y = bgr2$slope * sin(bgr2$aspect_rad)
)

plot_rng <- 32 # for x and y lims
 
axisLines <-
  data.frame(
    x = 0, y = 0,
    xend = c(plot_rng,0,-plot_rng,0),
    yend = c(0,plot_rng,0,-plot_rng))

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

bks = c(0.00008,0.0002,0.0004,0.0006,0.0008,0.001,.0012, 0.0014, 0.0016)
p2 <- ggplot(toCart) +
  stat_density_2d(data=toCart2,aes(x=x, y=y, fill=stat(level)), geom='polygon',contour=T, show.legend=F, breaks=bks) +
  #scale_fill_gradient2(low='white',high=demcb[5])  +   
  scale_fill_gradientn(colors = color1(20)[2:20]) +
  new_scale('fill') +
  stat_density_2d(data=toCart,aes(x=x, y=y, fill=stat(level)), geom='polygon',contour=T, show.legend=F, alpha=.4,breaks=bks) +
  #scale_fill_gradient2(low='white',high=demcb[22]) +
  scale_fill_gradientn(colors = color2(20)[5:20]) +
  #geom_point(data=toCart,aes(x=x,y=y),color=demcb[22],size=.05) +
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
  scale_y_continuous(name='Slope (°)',breaks = c(10,20,30),expand=c(0,0)) +
  scale_x_continuous(name=' ',expand=c(0,0)) +
  coord_cartesian(xlim=c(-plot_rng,plot_rng),ylim=c(-plot_rng,plot_rng)) +
  geom_segment(data = axisLines,
               aes(x = x, y = y,
                   xend = xend,
                   yend = yend),color='grey80') +
  geom_text(data = dirlabs,
            aes(x = xend, y = yend, label = angle,srt=ro)) +
  labs(tag = 'c')
p2

# 3. Save the combined plot:
#----------------------------------------------------------
jpeg('/Volumes/WDPassport/Rock_glacier_research/WUS/Figures/modeling_domain_plus2.jpg',units='in',width=12,height=8,res=500)
grid.arrange(g1, p1, p2, ncol=3, layout_matrix = cbind(c(1,1),c(1,1),c(2,3)))
dev.off()


# trying to understand density levels, here are a few helpful resources,
# although I still don't understand how this relates to my plots


#https://gis.stackexchange.com/questions/389739/how-to-interpret-kernel-density-maps
#https://groups.google.com/g/ggplot2/c/EZgs8DHTssU
#https://stackoverflow.com/questions/32206623/what-does-level-mean-in-ggplotstat-density2d/32207207

gg <- ggplot(toCart) +
  stat_density_2d(aes(x=x, y=y, fill=stat(level)), geom='polygon',contour=T, 
                  show.legend=F, n=3, bins=2) #,breaks=c(0.0001,0.0003,0.0006,0.0009,0.0012,.0015)) 
gg
gb <- ggplot_build(gg)
dim(gb$data[[1]])
head(gb$data[[1]])
bb <- gb$data[[1]]
unique(gb$data[[1]]$level)
unique(gb$data[[1]]$nlevel)

kd <- kde2d(toCart$x, toCart$y)
sum(kd$z) * (diff(kd$x)[1]*diff(kd$y)[1])



  