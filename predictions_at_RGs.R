
library(tidyverse)
library(viridis)
library(scico)

modeldir <- 'WUS/Data/Maxent_outputs/May-26-2021/'
samp <- read_csv('WUS/Data/Maxent_tables/sample.txt')

spred <- read_csv(paste0(modeldir,'species_samplePredictions.csv'))

samp <- cbind(samp,spred)
samp <- samp %>% dplyr::select(-(X:`Cumulative prediction`), -c(hw3,tmin,tmax,tschange,ppt,duration,maxswe))
names(samp) <- c('species','lon','lat','aspect','slope','headwall5','rocktype',
                 'tmean','solar','sfe','nosnowdays','pred')

# create custom colorbar:
thres <- 0.212
col1 <- rev(scico(round((1-thres)*100)+10, palette = 'devon'))
col1 <- col1[11:length(col1)] # trim white
col2 <- rev(scico(round(thres*100)+20, palette = 'lajolla'))
col2 <- col2[16:(length(col2)-5)]
colo <- c(col2,col1)


# map predictions at RG locations:
p1 <- ggplot(samp)+
  geom_point(aes(x=lon,y=lat,color=pred),size=.5,shape=21) +
  scale_color_gradientn(name='Suitability',
                        colors=colo) +
  borders('state') +
  coord_quickmap(xlim=c(-125,-104),ylim=c(32,49)) +
  theme_bw() +
  theme(legend.position = c(.12,.18),
        panel.grid = element_blank()) +
  labs(x=NULL, y=NULL)
p1


jpeg('WUS/Figures/predictions_at_RGs_preindustrial.jpeg',units='in',width=5,height=5,res=400)
p1 
dev.off()



# explain different prediction levels:
samp <- samp %>% mutate(grp = case_when(pred < 0.25 ~ 1,
                                        pred >= 0.25 & pred < 0.5 ~ 2,
                                        pred >=.5 & pred < 0.75 ~ 3,
                                        pred >= 0.75 ~ 4))
samplong <- samp %>% pivot_longer(cols=aspect:nosnowdays)                      

p2 <- ggplot(samplong) +
  geom_violin(aes(x=grp, y=value, group=grp)) +
  facet_wrap(~name, scales='free_y') +
  theme_bw() +
  theme(panel.grid.minor=element_blank()) +
  labs(x='quartile',y='covariate value')
p2

jpeg('WUS/Figures/RG_covariates_by_prediction_quartile.jpeg',units='in',width=6,height=5,res=400)
p2
dev.off()



### Present day predictions at RG locations ###
library(raster)

present <- raster(paste0(modeldir,'ctrl_predictions.tif'))
present <- extract(present, samp[,2:3])

samp$pred_present <- present

# how many rock glaciers are no longer in suitable habitat?
sum(present<thres)/length(present)

# map predictions at RG locations:
p1 <- ggplot(samp)+
  geom_point(aes(x=lon,y=lat,color=pred_present),size=.5,shape=21) +
  scale_color_gradientn(name='Present\nSuitability',
                        colors=colo) +
  borders('state') +
  coord_quickmap(xlim=c(-125,-104),ylim=c(32,49)) +
  theme_bw() +
  theme(legend.position = c(.12,.18),
        panel.grid = element_blank()) +
  labs(x=NULL, y=NULL)
p1

jpeg('WUS/Figures/predictions_at_RGs_present.jpeg',units='in',width=5,height=5,res=400)
p1 
dev.off()


### Future predictions at RG locations ###
pgw <- raster(paste0(modeldir,'pgw_predictions.tif'))
pgw <- extract(pgw, samp[,2:3])

samp$pred_pgw <- pgw

# how many rock glaciers are no longer in suitable habitat?
sum(pgw<thres)/length(pgw)

# map predictions at RG locations:
p1 <- ggplot(samp)+
  geom_point(aes(x=lon,y=lat,color=pred_pgw),size=.5,shape=21) +
  scale_color_gradientn(name='Future\nSuitability',
                        colors=colo) +
  borders('state') +
  coord_quickmap(xlim=c(-125,-104),ylim=c(32,49)) +
  theme_bw() +
  theme(legend.position = c(.12,.18),
        panel.grid = element_blank()) +
  labs(x=NULL, y=NULL)
p1

jpeg('WUS/Figures/predictions_at_RGs_pgw.jpeg',units='in',width=5,height=5,res=400)
p1 
dev.off()

### Compare pre-industrial predictions across RG classes ###
rg <- read_delim('Old/Data/Rock_glaciers/RG_Inventory_v2/RG.FOI_Inventory.txt',' ')
#rg <- rg %>% filter(type=='rockglacier') 

pre <- raster(paste0(modeldir,'preindustrial_predictions.tif'))
pre <- extract(pre, rg[,3:4])

rg$pre <- pre
rg <- rg %>% mutate(grp = case_when(activity==1 ~ 'active',
                                    activity==2 ~ 'inactive',
                                    is.na(activity) ~ 'feature of\ninterest'))
rg$grp <- factor(rg$grp, levels=c('active','inactive','feature of\ninterest'))

ggplot(rg) +
  geom_density(aes(x=pre,group=grp,fill=grp), alpha = 0.5) +
  labs(x='Pre-industrial Suitability') +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0)) +
  theme_bw() +
  theme(panel.grid=element_blank()) +
  scale_fill_discrete(name='Feature Type')

rg %>% group_by(grp) %>% summarize(mean(pre,na.rm=T))

