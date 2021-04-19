
library(tidyverse)

ee <- readRDS('WUS/Data/Maxent_outputs/Apr-09-2021/ENMeval_object.RData')

res <- read.table('WUS/Data/Maxent_outputs/Apr-09-2021/ENMeval_results.txt') 


quartz()
p1 <- res %>% 
ggplot() + 
  geom_line(aes(x=rm, y = AICc, color=features)) +
  xlab('regularization beta')

jpeg('WUS/Figures/model_complexity_AIC.jpeg',units='in',width=6,height=6,res=400)
p1
dev.off()

quartz()
ggplot(res) + 
  geom_line(aes(x=rm, y = train.AUC, color=features)) +
  xlab('regularization beta')

