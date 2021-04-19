# Plot  response curves

library(tidyverse)

dir <- 'WUS/Data/Maxent_outputs/Apr-11-2021_bestmodel/LQPH6/'


# import response curve data
a1 <- read_csv(paste0(dir,'plots/species_aspect_only.dat'))
a2 <- read_csv(paste0(dir,'plots/species_slope_only.dat'))
a3 <- read_csv(paste0(dir,'plots/species_duration_only.dat'))
a4 <- read_csv(paste0(dir,'plots/species_hw5_only.dat'))
a5 <- read_csv(paste0(dir,'plots/species_lith_only.dat'))
a6 <- read_csv(paste0(dir,'plots/species_nosnowdays_only.dat'))
a7 <- read_csv(paste0(dir,'plots/species_sfe_only.dat'))
a8 <- read_csv(paste0(dir,'plots/species_sw_only.dat'))
a9 <- read_csv(paste0(dir,'plots/species_tmean_only.dat'))
a10 <- read_csv(paste0(dir,'plots/species_tschange_only.dat'))

# filter out unreal conditions (e.g. negative aspect)
a1 <- a1 %>% filter(x>=0 & x<360)
a2 <- a2 %>% filter(x>=0)
a3 <- a3 %>% filter(x>=0 & x<=365)
a4 <- a4 %>% filter(x>=0 & x<=1)
a6 <- a6 %>% filter(x>=0)
a7 <- a7 %>% filter(x>=0)
a8 <- a8 %>% filter(x>=0)
a10 <- a10 %>% filter(x>=0)

# clean up names
a3$variable = 'snow duration'
a4$variable = 'headwall (5x5)'
a5$variable = 'lithology class'
a6$variable = 'no snow days'
a7$variable = 'SFE'
a8$variable = 'downward solar radiation'
a9$variable = 'mean annual temperature'
a10$variable = 'freeze-thaw cycles'

# bind covariate datasets together
rc <- rbind(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)

# order the covariates?
# tight axes


p1 <- rc %>%
  ggplot(aes(x=x,y=y)) +
  facet_wrap(~variable, scales='free_x', ncol=5) +
  geom_col(data = subset(rc, variable == 'lithology class')) +
  geom_line(data = subset(rc, variable != 'lithology class')) +
  xlab('covariate value') +
  ylab('predicted suitability') +
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) 

jpeg('WUS/Figures/response_curves_only.jpeg',units='in',width=8,height=5,res=400)
p1
dev.off()



