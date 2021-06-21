
# plot jackknife


library(tidyverse)

dir <- 'WUS/Data/Maxent_outputs/May-26-2021/'

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

p1 <- dat3 %>%
  ggplot() +
  geom_col(aes(x = shortname, y=value, group=grp, fill=grp), position='dodge') +
  geom_hline(aes(yintercept = regtraingain)) +
  #geom_col(data=fullmod, aes(x = name, y = value)) +
  ylab('regularized training gain') +
  xlab('') +
  theme_bw()+
  scale_y_continuous(expand = c(0, 0), limits = c(0,regtraingain+.1)) +
  scale_fill_manual(name='',values=c('grey80','grey40')) +
  theme(panel.grid.major.x=element_blank(),
        axis.text.x = element_text(angle = 0)) 

jpeg('WUS/Figures/jackknife.jpeg',units='in',width=8,height=5,res=400)
p1
dev.off()


