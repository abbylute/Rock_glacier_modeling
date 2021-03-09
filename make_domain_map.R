# Make Domain Map for WUS Rock Glacier study

library(raster)
library(ggplot2)
library(tidyverse)


rgd <- raster('/Volumes/WDPassport/Rock_glacier_research/WUS/Data/Domain/rg_domain.tif')
dem <- raster('/Volumes/WDPassport/DATA/DEM/NED/new/WUS_NED_210m.tif')
rgs <- read_csv('/Volumes/WDPassport/Rock_glacier_research/WUS/Data/Rock_glaciers/active_rgs.csv')

# Create elevation layer
dem <- crop(dem,rgd)
values(dem)[is.na(values(rgd))] <- NA
plot(dem)

xy <- coordinates(dem)
elev <- values(dem)
elevdf <- data.frame(xy)
elevdf$elev <- elev
elevdf <- elevdf %>% filter(!is.na(elev))
rm(dem,elev,xy,rgd);gc();


#mi <- read_csv('/Users/abbylute/Documents/IGERT/RockGlacierModeling/Data/Glacier/Millar_SierraNevada_RIF_Inventory/RIFDatabase_NSIDC.csv',skip =3)
#mi <- mi %>% filter(Activity == 'M') # only modern features, not relict


g1 <- ggplot() + 
  borders('state', fill='grey95') +
  geom_tile(data=elevdf, aes(x=x,y=y,color=elev)) +
  #borders('world') +
  geom_point(data=rgs, aes(x=lon,y=lat), size=.6) +
  #geom_point(data=mi, aes(x=Longitude,y=Latitude),size=.5,color='red') +
  coord_cartesian(x=c(-124.5,-104.5), y = c(31.5,49)) + 
  scale_color_gradientn(colors=terrain.colors(30), name = 'Elevation (m)') +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = c(0.1, 0.14)) +
  labs(x=NULL,y=NULL)


jpeg('/Volumes/WDPassport/Rock_glacier_research/WUS/Figures/modeling_domain.jpg',units='in',width=8,height=8,res=500)
g1 
dev.off()



