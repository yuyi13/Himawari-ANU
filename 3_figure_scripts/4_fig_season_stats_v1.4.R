# ################################################################
# objective:   generate plots for seasonal stats against in-situ
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

#######
## For the southern hemisphere temperate zone, 
## spring begins on 1 September, summer on 1 December, 
## autumn on 1 March, and winter on 1 June.

## draw the figure
library(ggsci)
SCICOL = pal_nejm('default', alpha=0.7)(5)

library(stringr)

ozfluxLL = read.csv('/g/data/os22/users/yu/himawari/0_code/OzFluxSites_LatLon.csv')

# drop HowardSprings and Litchfield
dropped_rows = which(ozfluxLL$sitename == 'HowardSprings' | ozfluxLL$sitename == 'Litchfield')
ozfluxLL = ozfluxLL[-dropped_rows,]

path0 = '/g/data/os22/users/yu/himawari/evaluations/0_in_situ_lst_standard_time/'
path1 = '/g/data/os22/users/yu/himawari/evaluations/anu_baseline_for_fitting_updated/'
path2 = '/g/data/os22/users/yu/himawari/evaluations/anu_v1.4.1/'
path3 = '/g/data/os22/users/yu/himawari/evaluations/copernicus/'

### ***** This is the boxplot for seasonal stats of bias *****

png('/g/data/os22/users/yu/himawari/figures/ZAC_seasonal_stats_bias.png', width=1800, height=1200)

m = rbind(c(1,2,3), c(4,5,6), c(7,8,9), c(10,11,12), c(13,14,15))
layout(m); par(mar = c(2, 4, 2, 2))

for (season_i in c('whole_year', 'spring', 'summer', 'autumn', 'winter')){

       if (season_i == 'spring') {season_month = c('09', '10', '11'); num_i = 2
} else if (season_i == 'summer') {season_month = c('12', '01', '02'); num_i = 3
} else if (season_i == 'autumn') {season_month = c('03', '04', '05'); num_i = 4
} else if (season_i == 'winter') {season_month = c('06', '07', '08'); num_i = 5
} else {season_month = c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12'); num_i = 1
} 

print(paste0('start to do the season ', season_i))

## calc the stats here

# t == 1: whole day; t == 2: daytime; t == 3: nighttime
for (t in 1:3){

  print(t)

  bias_ls_anu = c(); bias_ls_chiba = c(); bias_ls_coper = c(); bias_ls_anu_v1.4 = c()
  sample_number_total = c()

    for (k in 1:20){
    
    data1 = read.csv(paste0(path1, ozfluxLL$sitename[k], '_extracted_data.csv'))
    
    GMT_time = as.POSIXct(data1$local_time, tz=ozfluxLL$standard_timezone[k])
    attr(GMT_time, 'tzone') = 'GMT'

    data2 = read.csv(paste0(path2, ozfluxLL$sitename[k], '_extracted_data.csv'))
    data3 = read.csv(paste0(path3, ozfluxLL$sitename[k], '_extracted_data.csv'))
    
    # combine different dataframes
    df = cbind(data1, data2[,'anu_v1.4.1'], data3[,'copernicus'])
    colnames(df)[6:7] = c('anu_v1.4.1', 'copernicus')

    if (season_i != 'whole_year'){
      month_val = format(GMT_time, '%m')
      season_row = which(month_val == season_month[1] | month_val == season_month[2] | month_val == season_month[3])
      df = df[season_row,]
    }

    df = na.omit(df)

    if (t == 1) {

      print('whole day')
    } else if (t == 2) { ## daytime

      df = with(df, df[solar_angle <= 85, ])
      print('daytime')
    } else if (t == 3) { ## nighttime

      df = with(df, df[solar_angle > 85, ])
      print('nighttime')
    }

    bias_anu = mean(df$anu - df$flux); bias_ls_anu = c(bias_ls_anu, bias_anu)
    bias_chiba = mean(df$chiba - df$flux); bias_ls_chiba = c(bias_ls_chiba, bias_chiba)
    bias_coper = mean(df$copernicus - df$flux); bias_ls_coper = c(bias_ls_coper, bias_coper)
    bias_anu_v1.4 = mean(df$anu_v1.4 - df$flux); bias_ls_anu_v1.4 = c(bias_ls_anu_v1.4, bias_anu_v1.4)
    
    sample_number = nrow(df); sample_number_total = c(sample_number_total, sample_number)
    }

    print(paste0('ANU ', median(bias_ls_anu, na.rm=TRUE)))
    print(paste0('Chiba ', median(bias_ls_chiba, na.rm=TRUE)))
    print(paste0('Copernicus ', median(bias_ls_coper, na.rm=TRUE)))
    print(paste0('ANU_v1.4 ', median(bias_ls_anu_v1.4, na.rm=TRUE)))

    N_total = sum(sample_number_total)

    bias_anu_weighted_mean = sum(bias_ls_anu * sample_number_total) / N_total
    bias_chiba_weighted_mean = sum(bias_ls_chiba * sample_number_total) / N_total
    bias_coper_weighted_mean = sum(bias_ls_coper * sample_number_total) / N_total
    bias_anu_v1.4_weighted_mean = sum(bias_ls_anu_v1.4 * sample_number_total) / N_total

  boxplot(data.frame(anu = bias_ls_anu, chiba = bias_ls_chiba, copernicus = bias_ls_coper,
                    anu_v1.4 = bias_ls_anu_v1.4),  
                    col=SCICOL[c(1,2,3,5)], xaxt='n', ylim=c(-10,15), cex.axis=3); abline(a=0,b=0, lty='dashed')

  legend('topleft', legend=paste0('(', letters[(num_i-1)*3+t], ') Sample number = ', formatC(N_total, big.mark=',')), bty='n', cex=3)

  points(1:4, c(bias_anu_weighted_mean, bias_chiba_weighted_mean, bias_coper_weighted_mean, bias_anu_v1.4_weighted_mean), 
        col = 'red', pch = 19, cex = 3)
  text(1:4, -8, labels = format(round(c(bias_anu_weighted_mean, bias_chiba_weighted_mean, bias_coper_weighted_mean, bias_anu_v1.4_weighted_mean), 2), 
        nsmall = 2), col = 'red', cex = 3)

  }
}
dev.off()

# legend for the boxplot
library(ggsci)
SCICOL = pal_nejm('default', alpha=0.8)(5)

png('/g/data/os22/users/yu/himawari/figures/legend.png', width=1500, height=800)

plot(1:10, 1:10, col='white')
legend(1,2, legend = 'ANU', pch=15, col=SCICOL[1], bty='n', cex=3)
legend(2.5,2, legend = 'Chiba', pch=15, col=SCICOL[2], bty='n', cex=3)
legend(4.4,2, legend = 'Copernicus', pch=15, col=SCICOL[3], bty='n', cex=3)
legend(6.8,2, legend = 'ANUcalib', pch=15, col=SCICOL[5], bty='n', cex=3)

dev.off()


# This is the boxplot for seasonal stats of ubRMSE

png('/g/data/os22/users/yu/himawari/figures/ZAC_seasonal_stats_ubRMSE.png', width=1800, height=1200)

m = rbind(c(1,2,3), c(4,5,6), c(7,8,9), c(10,11,12), c(13,14,15))
layout(m); par(mar = c(2, 4, 2, 2))

for (season_i in c('whole_year', 'spring', 'summer', 'autumn', 'winter')){

       if (season_i == 'spring') {season_month = c('09', '10', '11'); num_i = 2
} else if (season_i == 'summer') {season_month = c('12', '01', '02'); num_i = 3
} else if (season_i == 'autumn') {season_month = c('03', '04', '05'); num_i = 4
} else if (season_i == 'winter') {season_month = c('06', '07', '08'); num_i = 5
} else {season_month = c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12'); num_i = 1
} 

print(paste0('start to do the season ', season_i))

## calc the stats here

for (t in 1:3){

  print(t)

  ubRMSE_ls_anu = c(); ubRMSE_ls_chiba = c(); ubRMSE_ls_coper = c(); ubRMSE_ls_anu_v1.4 = c()
  sample_number_total = c()

    for (k in 1:20){

    data1 = read.csv(paste0(path1, ozfluxLL$sitename[k], '_extracted_data.csv'))
    
    GMT_time = as.POSIXct(data1$local_time, tz=ozfluxLL$standard_timezone[k])
    attr(GMT_time, 'tzone') = 'GMT'

    data2 = read.csv(paste0(path2, ozfluxLL$sitename[k], '_extracted_data.csv'))
    data3 = read.csv(paste0(path3, ozfluxLL$sitename[k], '_extracted_data.csv'))
    
    # combine different dataframes
    df = cbind(data1, data2[,'anu_v1.4.1'], data3[,'copernicus'])
    colnames(df)[6:7] = c('anu_v1.4.1', 'copernicus')

    if (season_i != 'whole_year'){
      month_val = format(GMT_time, '%m')
      season_row = which(month_val == season_month[1] | month_val == season_month[2] | month_val == season_month[3])
      df = df[season_row,]
    }

    df = na.omit(df)

    if (t == 1) {

      print('whole day')
    } else if (t == 2) { ## daytime

      df = with(df, df[solar_angle <= 85, ])
      print('daytime')
    } else if (t == 3) { ## nighttime

      df = with(df, df[solar_angle > 85, ])
      print('nighttime')
    }

    ubRMSE_anu = sd(df$anu - df$flux); ubRMSE_ls_anu = c(ubRMSE_ls_anu, ubRMSE_anu)
    ubRMSE_chiba = sd(df$chiba - df$flux); ubRMSE_ls_chiba = c(ubRMSE_ls_chiba, ubRMSE_chiba)
    ubRMSE_coper = sd(df$copernicus - df$flux); ubRMSE_ls_coper = c(ubRMSE_ls_coper, ubRMSE_coper)
    ubRMSE_anu_v1.4 = sd(df$anu_v1.4 - df$flux); ubRMSE_ls_anu_v1.4 = c(ubRMSE_ls_anu_v1.4, ubRMSE_anu_v1.4)

    sample_number = nrow(df); sample_number_total = c(sample_number_total, sample_number)
    }

    print(paste0('ANU ', median(ubRMSE_ls_anu, na.rm=TRUE)))
    print(paste0('Chiba ', median(ubRMSE_ls_chiba, na.rm=TRUE)))
    print(paste0('Copernicus ', median(ubRMSE_ls_coper, na.rm=TRUE)))
    print(paste0('ANU_v1.4 ', median(ubRMSE_ls_anu_v1.4, na.rm=TRUE)))

    N_total = sum(sample_number_total)

    ubRMSE_anu_weighted_mean = sum(ubRMSE_ls_anu * sample_number_total) / N_total
    ubRMSE_chiba_weighted_mean = sum(ubRMSE_ls_chiba * sample_number_total) / N_total
    ubRMSE_coper_weighted_mean = sum(ubRMSE_ls_coper * sample_number_total) / N_total
    ubRMSE_anu_v1.4_weighted_mean = sum(ubRMSE_ls_anu_v1.4 * sample_number_total) / N_total

  boxplot(data.frame(anu = ubRMSE_ls_anu, chiba = ubRMSE_ls_chiba, copernicus = ubRMSE_ls_coper,
                    anu_v1.4 = ubRMSE_ls_anu_v1.4),  
                    col=SCICOL[c(1,2,3,5)], xaxt='n', ylim=c(0,8), cex.axis=3)

  legend('topleft', legend=paste0('(', letters[(num_i-1)*3+t], ') Sample number = ', formatC(N_total, big.mark=',')), bty='n', cex=3)

  points(1:4, c(ubRMSE_anu_weighted_mean, ubRMSE_chiba_weighted_mean, ubRMSE_coper_weighted_mean, ubRMSE_anu_v1.4_weighted_mean), 
        col = 'red', pch = 19, cex = 3)
  text(1:4, 0.5, labels = format(round(c(ubRMSE_anu_weighted_mean, ubRMSE_chiba_weighted_mean, ubRMSE_coper_weighted_mean, ubRMSE_anu_v1.4_weighted_mean), 2), 
        nsmall = 2), col='red', cex = 3)
  }

}
dev.off()


# This is the boxplot for seasonal stats against MODIS

library(ggsci)
SCICOL = pal_nejm('default', alpha=0.8)(5)

png('/g/data/os22/users/yu/himawari/figures/MODIS_seasonal_stats.png', width=1800, height=1200)

m = rbind(c(1,2,3), c(4,5,6), c(7,8,9), c(10,11,12), c(13,14,15))
layout(m); par(mar = c(2, 4, 2, 2))

for (season_i in c('whole_year', 'spring', 'summer', 'autumn', 'winter')){

       if (season_i == 'spring') {season_month = c('09', '10', '11')
} else if (season_i == 'summer') {season_month = c('12', '01', '02')
} else if (season_i == 'autumn') {season_month = c('03', '04', '05')
} else if (season_i == 'winter') {season_month = c('06', '07', '08')
} else {season_month = c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
}

print(paste0('start to do the season ', season_i))

## calc the stats here

    df = read.csv('/g/data/os22/users/yu/himawari/0_code/calib/modis_diff.csv')

    if (season_i != 'whole_year'){
      month_val = format(as.Date(df[,1]), '%m')
      season_row = which(month_val == season_month[1] | month_val == season_month[2] | month_val == season_month[3])
      df = df[season_row,]
    }

    df = na.omit(df)

    # bias
    boxplot(df[,2:5], col=SCICOL[c(1,2,3,5)], xaxt='n', ylim=c(-8,10), cex.axis=3)
    abline(a=0,b=0, lty='dashed')

    # ubRMSE 
    boxplot(df[,6:9], col=SCICOL[c(1,2,3,5)], xaxt='n', ylim=c(0,6), cex.axis=3)

    # R
    boxplot(df[,10:13], col=SCICOL[c(1,2,3,5)], xaxt='n', ylim=c(0.7,1), cex.axis=3)

}
dev.off()
