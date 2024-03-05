# ################################################################
# objective:   generate diurnal lst examples for a few
#              in-situ sites
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

path0 = '/g/data/os22/users/yu/himawari/evaluations/0_in_situ_lst_standard_time/'
path1 = '/g/data/os22/users/yu/himawari/evaluations/anu_baseline_for_fitting_updated/'
path2 = '/g/data/os22/users/yu/himawari/evaluations/anu_v1.4.1/'
path3 = '/g/data/os22/users/yu/himawari/evaluations/copernicus/'

# site lats and lons
ozfluxLL = read.csv('/g/data/os22/users/yu/himawari/0_code/OzFluxSites_LatLon.csv')

library(ggsci)
SCICOL = pal_nejm('default', alpha=0.8)(5)

for (t in 1:(as.integer(length(TOIs)/150))){

    duration = (1+(t-1)*150):(t*150)
    valid_pixels = nrow(na.omit(df[duration,]))

    if (valid_pixels > 135) print(t)
}

png('/g/data/os22/users/yu/himawari/figures/temporal_examples.png', height = 1600, width = 4000)

layout(rbind(c(1,2), c(3,4)), widths = c(1,1,1,1), heights = c(1,1,1,1))
par(mar = c(3, 3, 0.5, 0.5), cex=2)

for (x in 1:4){

    if (x == 1) {
        k = 5; t = 175 # CumberlandPlain
    } else if (x == 2){
        k = 6; t = 150 # DalyUncleared
    } else if (x == 3){
        k = 8; t = 116 # Gingin
    } else if (x == 4){
        k = 18; t = 120 # Tumbarumba
    }

    TOIs =  seq(ISOdatetime(2016,1,1,1,0,0,tz='GMT'),ISOdatetime(2020,12,31,23,0,0,tz='GMT'),3600)
    
    data0 = read.csv(paste0(path0, ozfluxLL$sitename[k], '_extracted_data.csv'))
    data1 = read.csv(paste0(path1, ozfluxLL$sitename[k], '_extracted_data.csv'))
    
    # use newly extracted flux data (data0) to replace data1
    data1$local_time = data0$local_time
    data1$flux = data0$flux

    data2 = read.csv(paste0(path2, ozfluxLL$sitename[k], '_extracted_data.csv'))
    data3 = read.csv(paste0(path3, ozfluxLL$sitename[k], '_extracted_data.csv'))

    df = cbind(data1, data2[,'anu_v1.4.1'], data3[,'copernicus'])
    colnames(df)[6:7] = c('anu_v1.4.1', 'copernicus')

    df$anu[which(is.na(df$chiba))] = NA
    df$anu_v1.4.1[which(is.na(df$chiba))] = NA

    duration = (1+(t-1)*150):(t*150)
    print(df$local_time[duration][1])
    print(df$local_time[duration][150])

    if (k == 5){
        df$anu[duration][148] = NA; df$chiba[duration][148] = NA
        df$anu_v1.4.1[duration][148] = NA
    }

    plot(as.POSIXct(df$local_time[duration]), df$flux[duration], ylim=c(270,330), type='l', xlab=NA, ylab =NA, cex.axis=2.2, lwd=4)
    lines(as.POSIXct(df$local_time[duration]), df$anu[duration], col=SCICOL[1], lwd=4)
    lines(as.POSIXct(df$local_time[duration]), df$chiba[duration], col=SCICOL[2], lwd=4)
    lines(as.POSIXct(df$local_time[duration]), df$copernicus[duration], col=SCICOL[3], lwd=4)
    lines(as.POSIXct(df$local_time[duration]), df$anu_v1.4.1[duration], col=SCICOL[5], lwd=4)

    legend('topleft', legend=paste0('(', letters[x], ') ', ozfluxLL$sitename[k]), cex=2.2, bty='n')
    legend('bottomleft', legend=paste0(df$local_time[duration][1], ' - ', df$local_time[duration][150]), cex=2.2, bty='n')

    points(as.POSIXct(df$local_time[duration]), df$flux[duration], pch=17, cex=1.5)
    points(as.POSIXct(df$local_time[duration]), df$flux[duration], pch=17, cex=1.5)
    points(as.POSIXct(df$local_time[duration]), df$anu[duration], col=SCICOL[1], pch=17, cex=1.5)
    points(as.POSIXct(df$local_time[duration]), df$chiba[duration], col=SCICOL[2], pch=17, cex=1.5)
    points(as.POSIXct(df$local_time[duration]), df$copernicus[duration], col=SCICOL[3], pch=17, cex=1.5)
    points(as.POSIXct(df$local_time[duration]), df$anu_v1.4.1[duration], col=SCICOL[5], pch=17, cex=1.5)
}

dev.off()

# legend for the boxplot
library(ggsci)
SCICOL = pal_nejm('default', alpha=0.8)(5)

png('/g/data/os22/users/yu/himawari/figures/legend.png', width=1500, height=800)

plot(1:10, 1:10, col='white')
legend(1,2, legend = 'ANU', pch=17, col=SCICOL[1], bty='n', cex=3)
legend(2.5,2, legend = 'Chiba', pch=17, col=SCICOL[2], bty='n', cex=3)
legend(4.4,2, legend = 'Copernicus', pch=17, col=SCICOL[3], bty='n', cex=3)
legend(6.8,2, legend = 'ANUcalib', pch=17, col=SCICOL[5], bty='n', cex=3)
legend(8.5,2, legend = 'In-situ', pch=17, col='black', bty='n', cex=3)

dev.off()
