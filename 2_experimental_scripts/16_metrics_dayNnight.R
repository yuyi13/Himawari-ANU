# ################################################################
# objective:   calculate in-situ metrics for day and night
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

ozfluxLL = read.csv('/g/data/os22/users/yu/himawari/0_code/OzFluxSites_LatLon.csv')

version = 'v1.4.1'

ofile = paste0('/g/data/os22/users/yu/himawari/evaluations/anu_metrics_dayNnight_', version, '.csv')
basic = data.frame(matrix(nrow=0, ncol=10))
colnames(basic) = c('Sitename','Bias_Ts_anu', 'Bias_Ts_v1.4', 'Bias_Ts_chiba', 'Bias_Ts_cop', 
                    'ubRMSE_Ts_anu', 'ubRMSE_Ts_v1.4', 'ubRMSE_Ts_chiba','ubRMSE_Ts_cop','sample_number')
write.table(basic, ofile, row.names=FALSE, sep=',', quote=FALSE)

path0 = '/g/data/os22/users/yu/himawari/evaluations/0_in_situ_lst_standard_time/'
path_baseline = '/g/data/os22/users/yu/himawari/evaluations/anu_baseline_for_fitting_updated/'
path1 = paste0('/g/data/os22/users/yu/himawari/evaluations/anu_', version, '/')
path2 = '/g/data/os22/users/yu/himawari/evaluations/copernicus/'

day_night = c('daytime', 'nighttime')

for (t in 1:2){
    for (k in 1:22){
    
    data00 = read.csv(paste0(path0, ozfluxLL$sitename[k], '_extracted_data.csv'))
    data0 = read.csv(paste0(path_baseline, ozfluxLL$sitename[k], '_extracted_data.csv'))
    
    # use newly extracted flux data (data00) to replace data0
    data0$local_time = data00$local_time
    data0$flux = data00$flux
    
    data1 = read.csv(paste0(path1, ozfluxLL$sitename[k], '_extracted_data.csv'))
    data2 = read.csv(paste0(path2, ozfluxLL$sitename[k], '_extracted_data.csv'))

    # incorporate the calib and copernicus
    df = cbind(data0, data1[,'anu_v1.4.1'], data2[,'copernicus'])
    colnames(df)[6:7] = c('anu_v1.4.1', 'copernicus')

    # figure out day and night
    df = na.omit(df)
    daytime = with(df, df[solar_angle <= 85, ])
    nighttime = with(df, df[solar_angle > 85, ])

    if (t == 1) (subdf = daytime) else (subdf = nighttime)

    sample_number = nrow(subdf)
    print(paste0('The samples number for ', ozfluxLL$sitename[k], ' is ', sample_number, ' with the version ', version))

    Ts = subdf$flux
    chiba = subdf$chiba
    cop = subdf$copernicus
    anu = subdf$anu
    v1.4 = subdf$anu_v1.4

    bias_anu = mean(anu - Ts)
    bias_v1.4 = mean(v1.4 - Ts)
    bias_chiba = mean(chiba - Ts)
    bias_cop = mean(cop - Ts)

    ubRMSE_anu = sd(anu - Ts)
    ubRMSE_v1.4 = sd(v1.4 - Ts)
    ubRMSE_chiba = sd(chiba - Ts)
    ubRMSE_cop = sd(cop - Ts)

    if (!is.na(bias_anu)){
    write.table(cbind(paste0(ozfluxLL$sitename[k], '_', day_night[t]),
                bias_anu,bias_v1.4,bias_chiba,bias_cop,
                ubRMSE_anu,ubRMSE_v1.4,ubRMSE_chiba,ubRMSE_cop,
                sample_number), 
                ofile, col.names=FALSE, row.names=FALSE, sep=',', quote=FALSE, append=TRUE)
    }
    }
}
