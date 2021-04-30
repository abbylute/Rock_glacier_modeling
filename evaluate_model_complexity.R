
library(tidyverse)

#ee <- readRDS('WUS/Data/Maxent_outputs/Apr-09-2021/ENMeval_object.RData')

#res <- read.table('WUS/Data/Maxent_outputs/Apr-09-2021/ENMeval_results.txt') 

# I used non-parallel code in calc_aic_across_complexities.R and ran on zaphod. took a full day. still doesn't look right.
res <- read.table('WUS/Data/AIC/aic_table.txt') 


quartz()
p1 <- res %>% 
ggplot() + 
  geom_line(aes(x=beta, y = AIC, color=feat)) +
  xlab('regularization beta')

jpeg('WUS/Figures/model_complexity_AIC.jpeg',units='in',width=6,height=6,res=400)
p1
dev.off()

quartz()
ggplot(res) + 
  geom_line(aes(x=rm, y = train.AUC, color=features)) +
  xlab('regularization beta')

