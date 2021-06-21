# Make combined plot with
# jackknife bar chart
# response curves



library(tidyverse)
library(gridExtra)

dir <- 'WUS/Data/Maxent_outputs/May-26-2021/'

###### Plot JackKnife ######
dat <- read_csv(paste0(dir,'maxentResults.csv'))
names(dat)
regtraingain <- dat$`Regularized training gain`
fullmod <- data.frame(name='full model',value=regtraingain)

dat1 <- dat %>% dplyr::select(starts_with('Training gain with'))
dat2 <- dat1 %>% pivot_longer(cols=1:18) %>%
  mutate(grp = if_else(str_detect(name, 'only'),'only','except')) 
dat3 <- dat2 %>%
  mutate(shortname = str_replace(name,'Training gain with','')) %>%
  mutate(shortname = str_replace(shortname,'out ','')) %>%
  mutate(shortname = str_replace(shortname,' only ','')) %>%
  mutate(grp = as.character(grp))

# rename variables CAREFULLY!
dat3$shortname[dat3$shortname == 'hw5'] <- 'headwall5'
dat3$shortname[dat3$shortname == 'lith'] <- 'rocktype'
dat3$shortname[dat3$shortname == 'sw'] <- 'solar'

levs <- dat3 %>% filter(grp=='only') %>% arrange(desc(value)) %>% dplyr::select(shortname)
levs
dat3$shortname <- factor(dat3$shortname, levels = c('tmean','headwall5','slope','sfe','rain','nosnowdays','aspect','rocktype','solar'))

# convert to normalized regularized training gain:
dat3$value <- dat3$value/regtraingain

p1 <- dat3 %>%
  ggplot() +
  geom_col(aes(x = shortname, y=value, group=grp, fill=grp), position='dodge') +
  geom_hline(aes(yintercept = 1)) +
  ylab('model performance') +
  xlab(NULL) +
  labs(tag = 'a') +
  theme_bw()+
  scale_y_continuous(expand = c(0, 0), limits = c(0,1.05)) +
  scale_fill_manual(name='',values=c('grey80','grey40')) +
  theme(panel.grid.major.x=element_blank(),
        axis.text.x = element_text(angle = 0)) 

#jpeg('WUS/Figures/jackknife.jpeg',units='in',width=8,height=5,res=400)
#p1
#dev.off()


###### PLOT RESPONSE CURVES ######
# For jackknife 2 panel plot, use marginal response curves
# make a separate plot for supplement of the other type of response curves

# import response curve data
a1 <- read_csv(paste0(dir,'plots/species_aspect.dat'))
a2 <- read_csv(paste0(dir,'plots/species_slope.dat'))
a4 <- read_csv(paste0(dir,'plots/species_hw5.dat'))
a5 <- read_csv(paste0(dir,'plots/species_lith.dat'))
a6 <- read_csv(paste0(dir,'plots/species_nosnowdays.dat'))
a7 <- read_csv(paste0(dir,'plots/species_sfe.dat'))
a8 <- read_csv(paste0(dir,'plots/species_sw.dat'))
a9 <- read_csv(paste0(dir,'plots/species_tmean.dat'))
a10 <- read_csv(paste0(dir,'plots/species_rain.dat'))

# filter out unreal conditions (e.g. negative aspect)
a1 <- a1 %>% filter(x>=0 & x<360)
a2 <- a2 %>% filter(x>=0)
a4 <- a4 %>% filter(x>=0 & x<=1)
a6 <- a6 %>% filter(x>=0)
a7 <- a7 %>% filter(x>=0)
a8 <- a8 %>% filter(x>=0)
a10 <- a10 %>% filter(x>=0)

# clean up names
a1$variable = 'aspect'
a2$variable = 'slope'
a4$variable = 'headwall5'
a5$variable = 'rocktype'
a6$variable = 'nosnowdays'
a7$variable = 'sfe'
a8$variable = 'solar'
a9$variable = 'tmean'
a10$variable = 'rain'

# bind covariate datasets together
rc <- rbind(a1,a2,a4,a5,a6,a7,a8,a9,a10)

rc$variable <- factor(rc$variable, levels = c('tmean','headwall5','slope','sfe','rain','nosnowdays','aspect','rocktype','solar'))

lbs = setNames(c("'tmean ' (degree*C)", 
                 "rain (mm)",
                 "'solar (W '*m^-2*')'", 
                 "sfe (mm)",
                 "nosnowdays",
                 "'aspect ' (degree)", 
                 "'slope ' (degree)", 
                 "headwall5",
                 "rocktype"),
               c('tmean','rain','solar','sfe','nosnowdays','aspect','slope','headwall5','rocktype'))[levels(rc$variable)]

p2 <- rc %>%
  ggplot(aes(x=x,y=y)) +
  facet_wrap(~variable, scales='free_x', ncol=5,
             labeller=as_labeller(lbs, label_parsed),
             strip.position = 'bottom') +
  geom_col(data = subset(rc, variable == 'rocktype')) +
  geom_line(data = subset(rc, variable != 'rocktype')) +
  xlab(NULL) +
  ylab('predicted suitability') +
  labs(tag = 'b') +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.placement = 'outside',
        strip.text = element_text(margin=margin(0,0,0,0,'cm'))) +
  scale_y_continuous(limits = c(0,1.05), expand = c(0,0))
p2


###### COMBINE PLOTS ######
jpeg('WUS/Figures/jackknife_and_response_curves.jpeg',units='in',width=8,height=7,res=400)
grid.arrange(p1,p2,nrow=2,heights=c(3,4))
dev.off()




#### PLot the other type of response curves as well ####
# import response curve data
a1 <- read_csv(paste0(dir,'plots/species_aspect_only.dat'))
a2 <- read_csv(paste0(dir,'plots/species_slope_only.dat'))
a4 <- read_csv(paste0(dir,'plots/species_hw5_only.dat'))
a5 <- read_csv(paste0(dir,'plots/species_lith_only.dat'))
a6 <- read_csv(paste0(dir,'plots/species_nosnowdays_only.dat'))
a7 <- read_csv(paste0(dir,'plots/species_sfe_only.dat'))
a8 <- read_csv(paste0(dir,'plots/species_sw_only.dat'))
a9 <- read_csv(paste0(dir,'plots/species_tmean_only.dat'))
a10 <- read_csv(paste0(dir,'plots/species_rain_only.dat'))

# filter out unreal conditions (e.g. negative aspect)
a1 <- a1 %>% filter(x>=0 & x<360)
a2 <- a2 %>% filter(x>=0)
a4 <- a4 %>% filter(x>=0 & x<=1)
a6 <- a6 %>% filter(x>=0)
a7 <- a7 %>% filter(x>=0)
a8 <- a8 %>% filter(x>=0)
a10 <- a10 %>% filter(x>=0)

# clean up names
a1$variable = 'aspect'
a2$variable = 'slope'
a4$variable = 'headwall5'
a5$variable = 'rocktype'
a6$variable = 'nosnowdays'
a7$variable = 'sfe'
a8$variable = 'solar'
a9$variable = 'tmean'
a10$variable = 'rain'

# bind covariate datasets together
rc <- rbind(a1,a2,a4,a5,a6,a7,a8,a9,a10)

rc$variable <- factor(rc$variable, levels = c('tmean','headwall5','slope','sfe','rain','nosnowdays','aspect','rocktype','solar'))

lbs = setNames(c("'tmean ' (degree*C)", 
                 "rain (mm)",
                 "'solar (W '*m^-2*')'", 
                 "sfe (mm)",
                 "nosnowdays",
                 "'aspect ' (degree)", 
                 "'slope ' (degree)", 
                 "headwall5",
                 "rocktype"),
               c('tmean','rain','solar','sfe','nosnowdays','aspect','slope','headwall5','rocktype'))[levels(rc$variable)]

p2 <- rc %>%
  ggplot(aes(x=x,y=y)) +
  facet_wrap(~variable, scales='free_x', ncol=5,
             labeller=as_labeller(lbs, label_parsed),
             strip.position = 'bottom') +
  geom_col(data = subset(rc, variable == 'rocktype')) +
  geom_line(data = subset(rc, variable != 'rocktype')) +
  xlab(NULL) +
  ylab('predicted suitability') +
  labs(tag = 'b') +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.placement = 'outside',
        strip.text = element_text(margin=margin(0,0,0,0,'cm'))) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0))

p2

jpeg('WUS/Figures/response_curves_only.jpeg',units='in',width=8,height=4.5,res=400)
p2
dev.off()



