# Plot  response curves

library(tidyverse)

dir <- 'WUS/Data/Maxent_outputs/May-26-2021/'


# import response curve data
a1 <- read_csv(paste0(dir,'plots/species_aspect_only.dat'))
a2 <- read_csv(paste0(dir,'plots/species_slope_only.dat'))
#a3 <- read_csv(paste0(dir,'plots/species_duration_only.dat'))
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
#a3 <- a3 %>% filter(x>=0 & x<=365)
a4 <- a4 %>% filter(x>=0 & x<=1)
a6 <- a6 %>% filter(x>=0)
a7 <- a7 %>% filter(x>=0)
a8 <- a8 %>% filter(x>=0)
a10 <- a10 %>% filter(x>=0)

# clean up names
#a3$variable = 'duration'
a1$variable = 'aspect (°C)'
a2$variable = 'slope (°C)'
a4$variable = 'headwall5'
a5$variable = 'rocktype'
a6$variable = 'nosnowdays'
a7$variable = 'sfe (mm)'
a8$variable = 'solar'
a9$variable = 'tmean (°C)'
a10$variable = 'rain (mm)'

# bind covariate datasets together
rc <- rbind(a1,a2,a4,a5,a6,a7,a8,a9,a10)

# order the covariates
rc$variable <- factor(rc$variable,
                      levels = c('tmean (°C)','rain (mm)','solar','sfe (mm)','nosnowdays','aspect (°C)','slope (°C)','headwall5','rocktype'))

# tight axes
lbs = setNames(c("'tmean ' (degree*C)", 
                 "rain (mm)",
                 "'solar (W '*m^-2*')'", 
                 "sfe (mm)",
                 "nosnowdays",
                 "'aspect ' (degree)", 
                 "'slope ' (degree)", 
                 "headwall5",
                 "rocktype"),
               c('tmean (°C)','rain (mm)','solar','sfe (mm)','nosnowdays','aspect (°C)','slope (°C)','headwall5','rocktype'))[levels(rc$variable)]

p1 <- rc %>%
  ggplot(aes(x=x,y=y)) +
  facet_wrap(~variable, scales='free_x', ncol=5,
             labeller=as_labeller(lbs, label_parsed)) +
  geom_col(data = subset(rc, variable == 'rocktype')) +
  geom_line(data = subset(rc, variable != 'rocktype')) +
  xlab(NULL) +
  ylab('predicted suitability') +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        strip.background = element_blank()) +
  scale_y_continuous(limits = c(0,1), expand = c(0,0))

p1

jpeg('WUS/Figures/response_curves_only.jpeg',units='in',width=8,height=4.5,res=400)
p1
dev.off()



