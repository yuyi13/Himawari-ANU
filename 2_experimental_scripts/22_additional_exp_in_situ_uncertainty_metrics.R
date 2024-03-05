# ################################################################
# objective:   calculate uncertainty of the in-situ data  
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

ofile = paste0('/datasets/work/d61-af-soilmoisture/work/himawari/in_situ_evaluations/OzFlux_uncertainty_dayNnight.csv')
basic = data.frame(matrix(nrow=0, ncol=5))
colnames(basic) = c('Sitename','Bias', 'ubRMSE', 'RMSE', 'sample_number')
write.table(basic, ofile, row.names=FALSE, sep=',', quote=FALSE)

path1 = '/datasets/work/d61-af-soilmoisture/work/himawari/in_situ_evaluations/anu_baseline_for_fitting_updated/'
path2 = '/datasets/work/d61-af-soilmoisture/work/himawari/in_situ_evaluations/in_situ_lst_with_random_noise/'

day_night = c('daytime', 'nighttime')

for (t in 1:2){
  for (k in 1:22){
    
    data1 = read.csv(paste0(path1, ozfluxLL$sitename[k], '_extracted_data.csv'))
    data2 = read.csv(paste0(path2, ozfluxLL$sitename[k], '_lst_with_noise.csv'))

    df = cbind(data1[,c('local_time', 'flux', 'solar_angle')], data2[,'flux'])
    colnames(df)[2:4] = c('flux_raw', 'solar_angle', 'flux_with_noise')

    # figure out day and night
    df = na.omit(df)
    daytime = with(df, df[solar_angle <= 85, ])
    nighttime = with(df, df[solar_angle > 85, ])

    if (t == 1) (subdf = daytime) else (subdf = nighttime)

    sample_number = nrow(subdf)
    print(paste0('The samples number for ', ozfluxLL$sitename[k], ' is ', sample_number))

    raw_ts = subdf$flux_raw
    ts_with_noise = subdf$flux_with_noise

    bias_val   = round(mean(ts_with_noise - raw_ts), 2)
    ubRMSE_val = round(sd(ts_with_noise - raw_ts), 2)
    RMSE_val   = round(RMSE(ts_with_noise, raw_ts), 2)

    if (!is.na(bias_val)){
    write.table(cbind(paste0(ozfluxLL$sitename[k], '_', day_night[t]),
                bias_val, ubRMSE_val, RMSE_val, sample_number), 
                ofile, col.names=FALSE, row.names=FALSE, sep=',', quote=FALSE, append=TRUE)
    }
    }
}
