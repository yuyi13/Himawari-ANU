# ################################################################
# objective:   assess the temporal window sensitivity 
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

library(stringr)
library(lubridate)

# function of RMSE
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

ozfluxLL = read.csv('/datasets/work/d61-af-soilmoisture/work/himawari/in_situ_evaluations/emis_lookup.csv')

ofile = paste0('/datasets/work/d61-af-soilmoisture/work/himawari/in_situ_evaluations/temporal_window_sensitivity.csv')
basic = data.frame(matrix(nrow=0, ncol=13))
colnames(basic) = c('Sitename', 'Bias_30min', 'ubRMSE_30min', 'sample_number', 'Bias_20min', 'ubRMSE_20min', 'sample_number',
                    'Bias_10min', 'ubRMSE_10min', 'sample_number', 'Bias_5min', 'ubRMSE_5min', 'sample_number')
write.table(basic, ofile, row.names=FALSE, sep=',', quote=FALSE)

path1 = '/datasets/work/d61-af-soilmoisture/work/himawari/in_situ_evaluations/anu_baseline_for_fitting_updated/'
path2 = '/datasets/work/d61-af-soilmoisture/work/himawari/in_situ_evaluations/extracted_modis/'

TOIs = seq(as.POSIXct('2016-01-01 01:00:00', tz='GMT'), as.POSIXct('2020-12-31 23:00:00', tz='GMT'), by=3600)

for (i in 1:22){
    
  data1 = read.csv(paste0(path1, ozfluxLL$sitename[i], '_extracted_data.csv'))
  data2 = read.csv(paste0(path2, ozfluxLL$sitename[i], '_extracted_modis_data.csv'))

  terra_hour_integer = as.integer(data2$terra_time_utc)
  terra_min_integer = as.integer((data2$terra_time_utc - terra_hour_integer) * 60)
  terra_time_utc = as.POSIXct(paste0(data2$date, ' ', terra_hour_integer, ':', terra_min_integer), format='%Y-%m-%d %H:%M', tz='GMT')

  aqua_hour_integer = as.integer(data2$aqua_time_utc)
  aqua_min_integer = as.integer((data2$aqua_time_utc - aqua_hour_integer) * 60)
  aqua_time_utc = as.POSIXct(paste0(data2$date, ' ', aqua_hour_integer, ':', aqua_min_integer), format='%Y-%m-%d %H:%M', tz='GMT')

  comparison_df = data.frame(modis_time_utc=c(terra_time_utc, aqua_time_utc), modis_lst=c(data2$terra_lst, data2$aqua_lst),
                            anu_filtered_30min=rep(NA, nrow(data2)*2), anu_filtered_20min=rep(NA, nrow(data2)*2),
                            anu_filtered_10min=rep(NA, nrow(data2)*2), anu_filtered_5min=rep(NA, nrow(data2)*2),
                            flux_filtered_30min=rep(NA, nrow(data2)*2), flux_filtered_20min=rep(NA, nrow(data2)*2),
                            flux_filtered_10min=rep(NA, nrow(data2)*2), flux_filtered_5min=rep(NA, nrow(data2)*2))
  comparison_df = comparison_df[order(comparison_df$modis_time_utc), ] # sort out comparison_df

  for (k in 1:nrow(comparison_df)){
    
    print(paste0('Processing ', ozfluxLL$sitename[i], ' ', k, ' out of ', nrow(comparison_df)))

    window_30min = which(TOIs <= comparison_df$modis_time_utc[k]+1800 & TOIs >= comparison_df$modis_time_utc[k]-1800)
    window_20min = which(TOIs <= comparison_df$modis_time_utc[k]+1200 & TOIs >= comparison_df$modis_time_utc[k]-1200)
    window_10min = which(TOIs <= comparison_df$modis_time_utc[k]+600 & TOIs >= comparison_df$modis_time_utc[k]-600)
    window_5min = which(TOIs <= comparison_df$modis_time_utc[k]+300 & TOIs >= comparison_df$modis_time_utc[k]-300)

    if (length(window_30min) > 0) comparison_df$anu_filtered_30min[k] = data1$anu[window_30min]
    if (length(window_20min) > 0) comparison_df$anu_filtered_20min[k] = data1$anu[window_20min]
    if (length(window_10min) > 0) comparison_df$anu_filtered_10min[k] = data1$anu[window_10min]
    if (length(window_5min) > 0) comparison_df$anu_filtered_5min[k] = data1$anu[window_5min]

    if (length(window_30min) > 0) comparison_df$flux_filtered_30min[k] = data1$flux[window_30min]
    if (length(window_20min) > 0) comparison_df$flux_filtered_20min[k] = data1$flux[window_20min]
    if (length(window_10min) > 0) comparison_df$flux_filtered_10min[k] = data1$flux[window_10min]
    if (length(window_5min) > 0) comparison_df$flux_filtered_5min[k] = data1$flux[window_5min]
  }

  bias_val_30min   = round(mean(comparison_df$anu_filtered_30min - comparison_df$modis_lst, na.rm=TRUE), 2)
  ubRMSE_val_30min = round(sd(comparison_df$anu_filtered_30min - comparison_df$modis_lst, na.rm=TRUE), 2)
  sample_30min     = length(na.omit(comparison_df$anu_filtered_30min))

  bias_val_20min   = round(mean(comparison_df$anu_filtered_20min - comparison_df$modis_lst, na.rm=TRUE), 2)
  ubRMSE_val_20min = round(sd(comparison_df$anu_filtered_20min - comparison_df$modis_lst, na.rm=TRUE), 2)
  sample_20min     = length(na.omit(comparison_df$anu_filtered_20min))

  bias_val_10min   = round(mean(comparison_df$anu_filtered_10min - comparison_df$modis_lst, na.rm=TRUE), 2)
  ubRMSE_val_10min = round(sd(comparison_df$anu_filtered_10min - comparison_df$modis_lst, na.rm=TRUE), 2)
  sample_10min     = length(na.omit(comparison_df$anu_filtered_10min))

  bias_val_5min    = round(mean(comparison_df$anu_filtered_5min - comparison_df$modis_lst, na.rm=TRUE), 2)
  ubRMSE_val_5min  = round(sd(comparison_df$anu_filtered_5min - comparison_df$modis_lst, na.rm=TRUE), 2)
  sample_5min      = length(na.omit(comparison_df$anu_filtered_5min))

  if (!is.na(bias_val_30min)){
  write.table(cbind(ozfluxLL$sitename[i], bias_val_30min, ubRMSE_val_30min, sample_30min, bias_val_20min, ubRMSE_val_20min, sample_20min,
                    bias_val_10min, ubRMSE_val_10min, sample_10min, bias_val_5min, ubRMSE_val_5min, sample_5min), 
                    ofile, col.names=FALSE, row.names=FALSE, sep=',', quote=FALSE, append=TRUE)
  }
}
