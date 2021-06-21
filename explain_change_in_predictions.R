# Explain changes in probability of rock glacier presence

# as written, this may not be telling me what I think it is since the places with increases
# could still have relatively low probabilities in the future scenario. Maybe look at the three part
# plotting (persist, disappear, enhance) first, and then come back to this.


library(tidyverse)
library(raster)


# load predictions
dir1 <- 'WUS/Data/Maxent_outputs/May-13-2021_wrain_notschange/' #Apr-21-2021_LQH/'
pred_ctrl <- raster(paste0(dir1,'preindustrial_predictions.tif'))
pred_pgw <- raster(paste0(dir1,'pgw_predictions.tif'))

xy <- coordinates(pred_ctrl)
pred_ctrl <- values(pred_ctrl)
pred_pgw <- values(pred_pgw)

pred_df <- data.frame('lon' = xy[,1],
                      'lat' = xy[,2],
                      'dpred' = pred_pgw-pred_ctrl)
pred_df <- pred_df %>% filter(!is.na(dpred))

rm(pred_ctrl, pred_pgw, xy); gc();


# create prediction groups
pred_df <- pred_df %>% mutate('grp' = case_when((dpred < -.2) ~ 'decrease',
                                                (dpred > 0.2) ~ 'increase'))

pred_df <- pred_df %>% filter(!is.na(grp))
pred_df$lon <- round(pred_df$lon, 6)
pred_df$lat <- round(pred_df$lat, 6)


# load covariate data
dir <- 'WUS/Data/Maxent_tables/'
cov_remove <- c('Id','hw3','tmin','tmax','maxswe','ppt','tschange')

ctrl <- read_csv(paste0(dir,'background.txt'))
#ctrl <- ctrl[1:100000,]
ctrl$rain <- ctrl$ppt - ctrl$sfe
ctrl <- ctrl %>% dplyr::select(-all_of(cov_remove))
ctrl <- ctrl %>% mutate('period'='ctrl')
ctrl$lon <- round(ctrl$lon, 6)
ctrl$lat <- round(ctrl$lat, 6)
ctrl <- left_join(pred_df, ctrl, by = c('lon','lat'))
ctrl <- ctrl %>% pivot_longer(cols = 'aspect':'rain',
                              names_to = 'covariate',
                              values_to = 'value')

pgw <- read_csv(paste0(dir,'background_PGW.txt'))
#pgw <- pgw[1:100000,]
pgw$rain <- pgw$ppt - pgw$sfe
pgw <- pgw %>% dplyr::select(-all_of(cov_remove))
pgw <- pgw %>% mutate('period'='pgw')
pgw$lon <- round(pgw$lon, 6)
pgw$lat <- round(pgw$lat, 6)
pgw <- left_join(pred_df, pgw, by = c('lon','lat'))
pgw <- pgw %>% pivot_longer(cols = 'aspect':'rain',
                              names_to = 'covariate',
                              values_to = 'value')

tab <- rbind(ctrl,pgw)
rm(ctrl,pgw);gc();

#tab$lon <- round(tab$lon, 6)
#tab$lat <- round(tab$lat, 6)
#tab <- left_join(tab,pred_df)

#tab <- tab %>% 
#  filter(!is.na(grp))

tab <- tab %>%
  mutate(per_grp = paste0(period,grp),
         per_grp = factor(per_grp,
                          levels =c("ctrldecrease","pgwdecrease","ctrlincrease","pgwincrease")))

# how many data points are there in the increase and decrease groups?
tab %>% filter(covariate=='aspect', period=='ctrl') %>% group_by(grp) %>% count()


# One figure looking at the distribution of time varying variables:
p1 <- tab %>% 
  filter(!covariate %in% c('aspect','hw5','lith','slope')) %>%
  ggplot() +
  geom_violin(aes(x = per_grp, y = value, color = period)) +
  facet_wrap(~covariate, scales = 'free_y') +
  scale_x_discrete(limits=c('ctrldecrease','pgwdecrease','ctrlincrease','pgwincrease'),
                   labels=c('decrease','','increase',''), name = NULL) +
  theme_bw() + theme(panel.grid = element_blank())
jpeg(paste0(dir1,'covariate_distributions_by_prediction_timevarying.jpeg'),units='in',width = 6,height=4,res=300)
p1
dev.off()
# locations that increased in RG probability were more likely to
# - have longer snow duration to begin with
# - have a few more no snow days
# - have much greater SFE
# - have much more freeze-thaw oscillations
# Temperature distributions between the two groups were hard to distinguish, which is interesting!

# One looking at static variables
p2 <- tab %>% 
  filter(covariate %in% c('aspect','hw5','lith','slope'),
         period == 'ctrl') %>%
  ggplot() +
  geom_violin(aes(x = per_grp, y = value, color = period), show.legend = F) +
  facet_wrap(~covariate, scales = 'free_y') +
  scale_x_discrete(limits = c('ctrldecrease','ctrlincrease'),
                   labels = c('decrease','increase'),
                   name = NULL) +
  theme_bw() + theme(panel.grid = element_blank())
jpeg(paste0(dir1,'covariate_distributions_by_prediction_timeconstant.jpeg'),units='in',width = 4,height=4,res=300)
p2
dev.off()

# locations that increased in RG probability were more likely to be on NE aspects with slightly greater slopes, 
# whereas those that saw decreased probability were more centered on N aspect with slightly lower slopes.
