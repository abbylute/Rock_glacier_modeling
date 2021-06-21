# suitable rock glacier habitat by ecoregion

library(tidyverse)
library(raster)
library(fasterize)
library(sf)
library(flextable) # https://ardata-fr.github.io/flextable-book/index.html
# other options for making tables:
# https://rfortherestofus.com/2019/11/how-to-make-beautiful-tables-in-r/



# load ecoregions
# https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/Eco_Level_III_US.pdf
eco <- st_read('/Volumes/WDPassport/DATA/Mapping/Ecoregions_level3/us_eco_l3.shp')

# load predictions
dir <- 'WUS/Data/Maxent_outputs/May-26-2021/'
# i reprojected the rasters on tesla, not laptop, to avoid running out of memory
# it was necessary to reproject them to use my previous method, but I think it is still good to 
# do so that the projection is equal area so that I can calculate area/cell
#pre <- raster(paste0(dir,'preindustrial_predictions.tif'))
#pre <- projectRaster(from = pre, crs = st_crs(eco)$proj4string)
#writeRaster(pre,paste0(dir,'preindustrial_predictions_ecocrs.tif'))
#ctrl <- raster(paste0(dir,'ctrl_predictions.tif'))
#ctrl <- projectRaster(from = ctrl, crs = st_crs(eco)$proj4string)
#writeRaster(ctrl,paste0(dir,'ctrl_predictions_ecocrs.tif'))
#pgw <- raster(paste0(dir,'pgw_predictions.tif'))
#pgw <- projectRaster(from = pgw, crs = st_crs(eco)$proj4string)
#writeRaster(pgw,paste0(dir,'pgw_predictions_ecocrs.tif'))
# load the reprojected versions:
pre <- raster(paste0(dir,'preindustrial_predictions_ecocrs.tif'))
ctrl <- raster(paste0(dir,'ctrl_predictions_ecocrs.tif'))
pgw <- raster(paste0(dir,'pgw_predictions_ecocrs.tif'))

# threshold
th <- 0.212 # no longer based on Liu et al., 2013

# area per cell in km2
apc <- (res(pre)[1]*res(pre)[2])/1000/1000


# rasterize the ecoregion sf
eco1 <- eco; eco1$US_L3CODE <- as.numeric(as.character(eco1$US_L3CODE))
eco_cropr <- fasterize(eco1, raster=pre, field='US_L3CODE', fun='last')
rm(eco1);gc();

eco_df <- as.data.frame(eco_cropr, xy=T)
names(eco_df) <- c('x','y','ecogrp')
rm(eco_cropr); gc();

pre_df <- as.data.frame(pre, xy=T)
names(pre_df) <- c('x','y','pre')
rm(pre);gc();

ctrl_df <- as.data.frame(ctrl, xy=T)
names(ctrl_df) <- c('x','y','ctrl')
rm(ctrl);gc();

pgw_df <- as.data.frame(pgw, xy=T)
names(pgw_df) <- c('x','y','pgw')
rm(pgw);gc();


# create a dataframe with rows for each point in the prediction rasters
#df <- left_join(eco_df, pre_df); rm(pre_df, eco_df);gc();
#df <- left_join(df, pgw_df); rm(pgw_df);gc();
# since x's and y's are the same, just bind instead of joining, which takes forever
identical(eco_df[,1:2],pre_df[,1:2])
df <- cbind(eco_df,pre_df$pre,ctrl_df$ctrl,pgw_df$pgw)
names(df) <- c('x','y','ecogrp','pre','ctrl','pgw')
rm(eco_df,pre_df,ctrl_df,pgw_df);gc();

# remove nas from ecogrp
df <- df %>% filter(!is.na(ecogrp))

# the future run has a few more NAs than the other run, probably because there were some NaNs in the snow data,
# which caused maxent to not model these locations. remove these locations so that area calculations match up 
# across time periods
f <- (is.na(df$pgw) & !is.na(df$pre)) # should sum to 324
df <- df[!f,]
rm(f);gc();



#### CALCULATE METRICS AND ADD TO OUTPUT TABLE ####


# total area 
tot <- df %>% group_by(ecogrp) %>% summarize(ncells = n()) %>% 
  mutate(total_area = ncells*apc) %>% dplyr::select(-ncells)

outdf <- tot

# modeled area
tot <- df %>% 
  group_by(ecogrp) %>% 
  summarize(ncells = sum(!is.na(pre))) %>%
  mutate(modeled_area = ncells*apc) %>% 
  dplyr::select(-ncells)

outdf <- outdf %>% left_join(tot)

# mean probability of modeled area
# Not sure if this is actually useful, ignore for now
#tot <- df %>% group_by(ecogrp) %>% 
#  summarize(ncells = sum(!is.na(pre)),
#            mean_pred_pre = mean(pre,na.rm=T),
#            mean_pred_pgw = mean(pgw,na.rm=T),
#            dmean_pred = mean_pred_pgw - mean_pred_pre) %>%
#  dplyr::select(-ncells)

#outdf <- outdf %>% left_join(tot)
# there are 324 cells where the pre prediction is a number but the pgw prediction is NA. They are mostly in 
# ecoregion 1, with a few in ecoregion 2. No cells have pgw predictions with NA for pre predictions. It could 
# be related to those areas not having any snow in the future, which led to NAs in the maxent dataframes,
# which were excluded from prediction. Not ideal, but not a big problem either.

# amount of area that exceeds/falls below the threshold
tot <- df %>% group_by(ecogrp) %>%
  summarize(area_suitable_pre = sum(pre>th, na.rm=T)*apc,
            area_suitable_ctrl = sum(ctrl>th, na.rm=T)*apc,
            area_suitable_pgw = sum(pgw>th, na.rm=T)*apc,
            delt_area_suitable12 = area_suitable_ctrl - area_suitable_pre,
            delt_area_suitable23 = area_suitable_pgw - area_suitable_ctrl,
            pdelt_area_suitable12 = (area_suitable_ctrl - area_suitable_pre)/area_suitable_pre*100,
            pdelt_area_suitable23 = (area_suitable_pgw - area_suitable_ctrl)/area_suitable_ctrl*100,
            area_unsuitable_pre = sum(pre<th, na.rm=T)*apc,
            area_unsuitable_ctrl = sum(ctrl<th, na.rm=T)*apc,
            area_unsuitable_pgw = sum(pgw<th, na.rm=T)*apc,
            delt_area_unsuitable12 = area_unsuitable_ctrl - area_unsuitable_pre,
            delt_area_unsuitable23 = area_unsuitable_pgw - area_unsuitable_ctrl,
            pdelt_area_unsuitable12 = (area_unsuitable_ctrl - area_unsuitable_pre)/area_unsuitable_pre*100,
            pdelt_area_unsuitable23 = (area_unsuitable_pgw - area_unsuitable_ctrl)/area_unsuitable_ctrl*100)
outdf <- outdf %>% left_join(tot)
# if modeled area is 0, set all of these new values to NaN
# also if pdelt had to divide by 0, set to 0
outdf <- outdf %>% 
  mutate(area_suitable_pre = if_else(modeled_area==0,NaN,area_suitable_pre),
         area_suitable_ctrl = if_else(modeled_area==0,NaN,area_suitable_ctrl),
         area_suitable_pgw = if_else(modeled_area==0,NaN,area_suitable_pgw),
         delt_area_suitable12 = if_else(modeled_area==0,NaN,delt_area_suitable12),
         delt_area_suitable23 = if_else(modeled_area==0,NaN,delt_area_suitable23),
         pdelt_area_suitable12 = if_else(modeled_area==0,NaN,pdelt_area_suitable12),
         pdelt_area_suitable12 = if_else(area_suitable_pre==0 & area_suitable_ctrl==0,0,pdelt_area_suitable12),
         pdelt_area_suitable23 = if_else(modeled_area==0,NaN,pdelt_area_suitable23),
         pdelt_area_suitable23 = if_else(area_suitable_ctrl==0 & area_suitable_pgw==0,0,pdelt_area_suitable23),
         area_unsuitable_pre = if_else(modeled_area==0,NaN,area_unsuitable_pre),
         area_unsuitable_ctrl = if_else(modeled_area==0,NaN,area_unsuitable_ctrl),
         area_unsuitable_pgw = if_else(modeled_area==0,NaN,area_unsuitable_pgw),
         delt_area_unsuitable12 = if_else(modeled_area==0,NaN,delt_area_unsuitable12),
         delt_area_unsuitable23 = if_else(modeled_area==0,NaN,delt_area_unsuitable23),
         pdelt_area_unsuitable12 = if_else(modeled_area==0,NaN,pdelt_area_unsuitable12),
         pdelt_area_unsuitable12 = if_else(area_unsuitable_pre==0 & area_unsuitable_ctrl==0,0,pdelt_area_unsuitable12),
         pdelt_area_unsuitable23 = if_else(modeled_area==0,NaN,pdelt_area_unsuitable23),
         pdelt_area_unsuitable23 = if_else(area_unsuitable_ctrl==0 & area_unsuitable_pgw==0,0,pdelt_area_unsuitable23))

### CLEAN UP THE DATAFRAME ###

# Apply rounding
outdf <- outdf %>%
  mutate_at(vars(!starts_with('eco') & !starts_with('pdelt')), round, 2) %>%
  mutate_at(vars(starts_with('pdelt')), round, 1) 

# Remove a few regions that are too much out the domain
outdf <- outdf %>% filter(!ecogrp %in% c(27,30,44,46))

# Add ecoregion names column
econm <- st_set_geometry(eco[,1:2], NULL)
econm <- econm %>% distinct() %>% mutate(US_L3CODE = as.numeric(as.character(US_L3CODE)))
outdf <- outdf %>% 
  left_join(econm, by=c('ecogrp'='US_L3CODE')) %>% 
  relocate('US_L3NAME',.after=ecogrp) 
rm(econm);gc()

# Create totals
outdf$ecogrp <- as.character(outdf$ecogrp)
#nr <- nrow(outdf)
#tr <- nr + 1 # totals row
outdf <- outdf %>% add_row(ecogrp = 'Total', 
                  US_L3NAME = 'Western US',
                  total_area = sum(outdf$total_area),
                  modeled_area = sum(outdf$modeled_area, na.rm=T),
                  area_suitable_pre = sum(outdf$area_suitable_pre, na.rm=T),
                  area_suitable_ctrl = sum(outdf$area_suitable_ctrl, na.rm=T),
                  area_suitable_pgw = sum(outdf$area_suitable_pgw, na.rm=T),
                  delt_area_suitable12 = sum(outdf$delt_area_suitable12, na.rm=T),
                  delt_area_suitable23 = sum(outdf$delt_area_suitable23, na.rm=T),
                  area_unsuitable_pre = sum(outdf$area_unsuitable_pre, na.rm=T),
                  area_unsuitable_ctrl = sum(outdf$area_unsuitable_ctrl, na.rm=T),
                  area_unsuitable_pgw = sum(outdf$area_unsuitable_pgw, na.rm=T),
                  delt_area_unsuitable12 = sum(outdf$delt_area_unsuitable12, na.rm=T),
                  delt_area_unsuitable23 = sum(outdf$delt_area_unsuitable23, na.rm=T)) #%>% View()
# compute total pdelts based on area totals, not average of pdelts:
nr <- nrow(outdf)
outdf$pdelt_area_suitable12[nr] <- round((outdf$area_suitable_ctrl[nr]-outdf$area_suitable_pre[nr])/outdf$area_suitable_pre[nr]*100,1)
outdf$pdelt_area_suitable23[nr] <- round((outdf$area_suitable_pgw[nr]-outdf$area_suitable_ctrl[nr])/outdf$area_suitable_ctrl[nr]*100,1)
outdf$pdelt_area_unsuitable12[nr] <- round((outdf$area_unsuitable_ctrl[nr]-outdf$area_unsuitable_pre[nr])/outdf$area_unsuitable_pre[nr]*100,1)
outdf$pdelt_area_unsuitable23[nr] <- round((outdf$area_unsuitable_pgw[nr]-outdf$area_unsuitable_ctrl[nr])/outdf$area_unsuitable_ctrl[nr]*100,1)

# Improve column names
names(outdf) <- c('Ecoregion Number',
                  'Ecoregion Name',
                  'Total',#Total Area',
                  'Modeled',#'Modeled Area',
                  'Preindustrial1',
                  'Historical1',# Suitable Area',
                  'Future1',# Suitable Area',
                  'Absolute Change Pre-Hist1',# in Suitable Area',
                  'Absolute Change Hist-Fut1',
                  'Percent Change Pre-Hist1',# in Suitable Area',
                  'Percent Change Hist-Fut1',
                  'Preindustrial2',
                  'Historical2',# Unsuitable Area',
                  'Future2',# Unsuitable Area',
                  'Absolute Change Pre-Hist2',# in Unsuitable Area',
                  'Absolute Change Hist-Fut2',
                  'Percent Change Pre-Hist2',
                  'Percent Change Hist-Fut2')# in Unsuitable Area')


ft <- flextable(outdf) %>% colformat_num(na_str = '-')
ft <- set_header_labels(ft, 
                        Preindustrial1='Pre-industrial',Preindustrial2='Pre-industrial',
                        Historical1='Historical',Historical2='Historical',
                        Future1='Future', Future2='Future',
                        `Absolute Change Pre-Hist1`='Absolute Change Pre-Hist', `Absolute Change Pre-Hist2`='Absolute Change Pre-Hist',
                        `Absolute Change Hist-Fut1`='Absolute Change Hist-Fut', `Absolute Change Hist-Fut2`='Absolute Change Hist-Fut',
                        `Percent Change Pre-Hist1`='Percent Change Pre-Hist', `Percent Change Pre-Hist2`='Percent Change Pre-Hist',
                        `Percent Change Hist-Fut1`='Percent Change Hist-Fut', `Percent Change Hist-Fut2`='Percent Change Hist-Fut')
ft <- add_header_row(ft, colwidths=c(4,7,7), values=c(' ','Suitable','Unsuitable'))
ft <- hline(ft, i=nrow(outdf)-1, border = fp_border_default(width=2))
ft <- vline(ft, j=c(4,11))
ft

library(officer) # for prop_section
save_as_docx('Rock Glacier Modeling Summary Table' = ft, 
             path = 'WUS/Figures/RG_summary_table.docx',
             pr_section = prop_section(page_size = page_size(width=10,height=7,orient = 'landscape')))
library(webshot)
# png and jpeg are not exporting correctly...
save_as_image(ft, path = 'WUS/Figures/RG_summary_table.jpeg')
# but pdf works
save_as_image(ft, zoom = 1, path = 'WUS/Figures/RG_summary_table.pdf')


