
# plot distribution of rg and bg covariates for each covariate
library(tidyverse)

indir <- 'WUS/Data/Maxent_tables/'

samp <- read_csv(paste0(indir,'sample.txt'))
bgr <- read_csv(paste0(indir,'background.txt'))

samp <- samp[,4:ncol(samp)]
samp$grp = 'rock glaciers'
bgr <- bgr[,4:ncol(bgr)]
bgr$grp <- 'background'

# combine rgs and background in one df
tab <- rbind(samp,bgr)

# remove variables we won't be using in modeling
tab <- tab %>% dplyr::select(-c(hw3,tmin,tmax,ppt,maxswe))

# pivot longer to plot
tab2 <- tab %>% pivot_longer(aspect:nosnowdays, names_to='covariate', values_to='value')

quartz()
p1 <- tab2 %>%  
  ggplot() +
  geom_density(aes(value, color=grp), trim =T) +
  facet_wrap(~covariate, scales='free') +
  #theme_minimal() +
  scale_color_discrete(name='')

jpeg('WUS/Figures/covariate_distributions.jpeg',units = 'in', width = 12, height = 10, res = 300)
p1
dev.off()
