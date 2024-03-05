# ################################################################
# objective:   generate hourly bias plots against in-situ
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

path0 = '/g/data/os22/users/yu/himawari/evaluations/modis/'
path1 = '/g/data/os22/users/yu/himawari/evaluations/anu_baseline_for_fitting_updated/'
path2 = '/g/data/os22/users/yu/himawari/evaluations/anu_v1.4.1/'
path3 = '/g/data/os22/users/yu/himawari/evaluations/copernicus/'

# site lats and lons
ozfluxLL = read.csv('/g/data/os22/users/yu/himawari/0_code/OzFluxSites_LatLon.csv')

# drop HowardSprings and Litchfield
dropped_rows = which(ozfluxLL$sitename == 'HowardSprings' | ozfluxLL$sitename == 'Litchfield')
ozfluxLL = ozfluxLL[-dropped_rows,]

# nrow: site number; ncol: hour
global_df_anu = data.frame(matrix(nrow=20,ncol=24))
colnames(global_df_anu) = 0:23
global_df_chiba = global_df_v1.4.1 = global_df_cop = global_df_anu

TOIs =  seq(ISOdatetime(2016,1,1,1,0,0,tz='GMT'),ISOdatetime(2020,12,31,23,0,0,tz='GMT'),3600)

for (k in 1:nrow(ozfluxLL)){

    data0 = read.csv(paste0(path0, ozfluxLL$sitename[k], '_extracted_data.csv'))
    data1 = read.csv(paste0(path1, ozfluxLL$sitename[k], '_extracted_data.csv'))
    data2 = read.csv(paste0(path2, ozfluxLL$sitename[k], '_extracted_data.csv'))
    data3 = read.csv(paste0(path3, ozfluxLL$sitename[k], '_extracted_data.csv'))

    # combine different dataframes
    df = cbind(data1, data2[,'anu_v1.4.1'], data3[,'copernicus'])
    colnames(df)[6:7] = c('anu_v1.4.1', 'copernicus')
    
    # need formatted time
    standard_time = TOIs; attr(standard_time, 'tzone') = ozfluxLL$standard_timezone[k]
    df$local_time = standard_time
    df = na.omit(df)

    # test if the time is continous
    # time_diff = diff(as.POSIXct(df$local_time), 1, 1)
    # which(time_diff == 0)

    # calc the hourly difference for everyday
    df_diff_anu = data.frame(local_time = as.numeric(format(df$local_time, '%H')), 
                            diff = df$anu - df$flux)
    
    df_diff_chiba = data.frame(local_time = as.numeric(format(df$local_time, '%H')), 
                            diff = df$chiba - df$flux)
    
    df_diff_v1.4.1 = data.frame(local_time = as.numeric(format(df$local_time, '%H')),
                            diff = df$anu_v1.4.1 - df$flux)

    df_diff_cop = data.frame(local_time = as.numeric(format(df$local_time, '%H')),
                            diff = df$copernicus - df$flux)

    # group by hour for the period of 2016-2020
    for (t in 0:23){

        select_df_anu = with(df_diff_anu, df_diff_anu[local_time == t,])
        global_df_anu[k,t+1] = mean(select_df_anu$diff)

        select_df_chiba = with(df_diff_chiba, df_diff_chiba[local_time == t,])
        global_df_chiba[k,t+1] = mean(select_df_chiba$diff)

        select_df_v1.4.1 = with(df_diff_v1.4.1, df_diff_v1.4.1[local_time == t,])
        global_df_v1.4.1[k,t+1] = mean(select_df_v1.4.1$diff)

        select_df_cop = with(df_diff_cop, df_diff_cop[local_time == t,])
        global_df_cop[k,t+1] = mean(select_df_cop$diff)
    }
}

## ***** Here to plot all four data *****
## https://stackoverflow.com/questions/14604439/plot-multiple-boxplot-in-one-graph

library(ggsci)
SCICOL = pal_nejm('default', alpha=0.7)(5)

png('/g/data/os22/users/yu/himawari/figures/hourly_err_2016-2020.png', height = 800, width = 2000)

boxplot(global_df_anu, cex.axis = 2.5, boxfill=SCICOL[1], ylim=c(-8,12), 
        boxwex=0.25, at = 0:23 - 0.2) # shift these left by -0.3

boxplot(global_df_chiba, xaxt = 'n', yaxt = 'n', add = TRUE, boxfill=SCICOL[2], 
        boxwex=0.25, at = 0:23) # position unchanged

boxplot(global_df_cop, xaxt = 'n', yaxt = 'n', add = TRUE, boxfill=SCICOL[3],
        boxwex=0.25, at = 0:23 + 0.2) # shift to the right by +0.3

boxplot(global_df_v1.4.1, xaxt = 'n', yaxt = 'n', add = TRUE, boxfill=SCICOL[5],
        boxwex=0.25, at = 0:23 + 0.4) # shift to the right by +0.6

abline(h=0, col='black', lty=2, lwd = 2)

dev.off()
