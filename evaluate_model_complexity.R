
library(tidyverse)
library(RColorBrewer)



resnew <- read.table('WUS/Data/AIC/aic_table_May24.txt')
resnew$feat <- factor(resnew$feat, levels = c('L','LQ','LT','LH','LQH','LQT','LQTH'))

#colo <- brewer.pal(11,'Spectral')
#ggplot()+ geom_point(aes(x=1:11,y=1:11,color=as.character(1:11))) +scale_color_manual(values=colo)
#colo <- colo[c(1,3,5,6,8,10,11)]
#colo[4] <- 'goldenrod'

colo <- brewer.pal(9,'Set1')
colo <- colo[c(1:5,7,9)]

colo <- brewer.pal(7,'Set2')
p1 <- resnew %>% 
ggplot() + 
  geom_line(aes(x=beta, y = AICc, color=feat), size=1.1) +
  scale_color_manual(name='feature\nclasses', 
                     values = colo) +
  xlab('regularization beta') +
  theme_bw()

jpeg('WUS/Figures/model_complexity_AIC.jpeg',units='in',width=6,height=6,res=400)
p1
dev.off()

