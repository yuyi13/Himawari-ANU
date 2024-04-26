# ################################################################
# objective:   generate additional figures requested by reviewers
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

#################################
# plot directionality experiments
#################################

source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_SetupForGraphics.R')
library(pals); library(fields); library(terra); library(RColorBrewer)
spectralRamp = colorRampPalette(RColorBrewer::brewer.pal(11, 'Spectral'))

directionality_ratio = function(geo_vza, geo_vaa, sza, saa, par_a, par_d){
    
    geo_relative_aa = saa - geo_vaa
    geo_relative_aa[geo_relative_aa < 0] = geo_relative_aa[geo_relative_aa < 0] + 360
    geo_relative_aa[geo_relative_aa > 360] = geo_relative_aa[geo_relative_aa > 360] - 360
    geo_relative_aa = abs(geo_relative_aa - 180)

    geo_emis_kernel  = 1 - cos(geo_vza * pi/180)
    geo_solar_kernel = sin(geo_vza * pi/180) * cos(sza * pi/180) * sin(sza * pi/180) * cos((sza - geo_vza) * pi/180) * cos(geo_relative_aa * pi/180)
    
    ratio = 1 + par_a * geo_emis_kernel + par_d * geo_solar_kernel

    return(ratio)
}

directionality_path = '/datasets/work/d61-af-soilmoisture/work/himawari/directionality_par/Nelder_Mead_allVZA/'

baseline_par_a = raster(paste0(directionality_path, 'baseline/par_a_mosaic_1to99percentile.tif'))
baseline_par_d = raster(paste0(directionality_path, 'baseline/par_d_mosaic_1to99percentile.tif'))

chiba_par_a = raster(paste0(directionality_path, 'chiba/par_a_mosaic_1to99percentile.tif'))
chiba_par_d = raster(paste0(directionality_path, 'chiba/par_d_mosaic_1to99percentile.tif'))

copernicus_par_a = raster(paste0(directionality_path, 'copernicus/par_a_mosaic_1to99percentile.tif'))
copernicus_par_d = raster(paste0(directionality_path, 'copernicus/par_d_mosaic_1to99percentile.tif'))

anu_calib_par_a = raster(paste0(directionality_path, 'anu_calib/par_a_mosaic_1to99percentile.tif'))
anu_calib_par_d = raster(paste0(directionality_path, 'anu_calib/par_d_mosaic_1to99percentile.tif'))

# now read several variables
Dates = as.Date('2016-12-21')

aus_template = raster(extent(112, 154, -45, -10), res=0.02, crs=PROJ_LATLON)

h8_vza_rst = raster('/datasets/work/d61-af-soilmoisture/work/himawari/ancillary/20150127000000-P1S-ABOM_GEOM_SENSOR-PRJ_GEOS141_2000-HIMAWARI8-AHI.nc', varname='sensor_zenith_angle')
h8_vza_rst = projectRaster(h8_vza_rst, aus_template, method='ngb')

h8_vaa_rst = raster('/datasets/work/d61-af-soilmoisture/work/himawari/ancillary/20150127000000-P1S-ABOM_GEOM_SENSOR-PRJ_GEOS141_2000-HIMAWARI8-AHI.nc', varname='sensor_azimuth_angle')
h8_vaa_rst = projectRaster(h8_vaa_rst, aus_template, method='ngb')

sza = raster(paste0('/datasets/work/d61-af-soilmoisture/work/SatelliteLST/Himawari_SZA/', format(Dates, '%Y/%m/%d/%Y%m%d0200_Himawari_SolarZenithAngle_AusSubset.tif')))
saa = raster(paste0('/datasets/work/d61-af-soilmoisture/work/SatelliteLST/Himawari_SAA/', format(Dates, '%Y/%m/%d/%Y%m%d0200_Himawari_SolarAzimuthAngle_AusSubset.tif')))

# calculate the directionality ratio
baseline_ratio = directionality_ratio(h8_vza_rst, h8_vaa_rst, sza, saa, baseline_par_a, baseline_par_d)
chiba_ratio = directionality_ratio(h8_vza_rst, h8_vaa_rst, sza, saa, chiba_par_a, chiba_par_d)
copernicus_ratio = directionality_ratio(h8_vza_rst, h8_vaa_rst, sza, saa, copernicus_par_a, copernicus_par_d)
anu_calib_ratio = directionality_ratio(h8_vza_rst, h8_vaa_rst, sza, saa, anu_calib_par_a, anu_calib_par_d)

#baseline_ratio[baseline_ratio > 1.01] = 1.01; baseline_ratio[baseline_ratio < 0.99] = 0.99
#chiba_ratio[chiba_ratio > 1.01] = 1.01; chiba_ratio[chiba_ratio < 0.99] = 0.99
#copernicus_ratio[copernicus_ratio > 1.01] = 1.01; copernicus_ratio[copernicus_ratio < 0.99] = 0.99
#anu_calib_ratio[anu_calib_ratio > 1.01] = 1.01; anu_calib_ratio[anu_calib_ratio < 0.99] = 0.99

# mask out some outliers
baseline_par_a[baseline_par_a > 0.04] = 0.04; baseline_par_a[baseline_par_a < (-0.04)] = -0.04
baseline_par_d[baseline_par_d > 0.04] = 0.04; baseline_par_d[baseline_par_d < (-0.04)] = -0.04

chiba_par_a[chiba_par_a > 0.04] = 0.04; chiba_par_a[chiba_par_a < (-0.04)] = -0.04
chiba_par_d[chiba_par_d > 0.04] = 0.04; chiba_par_d[chiba_par_d < (-0.04)] = -0.04

copernicus_par_a[copernicus_par_a > 0.04] = 0.04; copernicus_par_a[copernicus_par_a < (-0.04)] = -0.04
copernicus_par_d[copernicus_par_d > 0.04] = 0.04; copernicus_par_d[copernicus_par_d < (-0.04)] = -0.04

anu_calib_par_a[anu_calib_par_a > 0.04] = 0.04; anu_calib_par_a[anu_calib_par_a < (-0.04)] = -0.04
anu_calib_par_d[anu_calib_par_d > 0.04] = 0.04; anu_calib_par_d[anu_calib_par_d < (-0.04)] = -0.04

# make legends for par and ratio
par(cex=2.5)
image.plot(baseline_par_a, col=spectralRamp(64), zlim=c(-0.04,0.04), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
par(cex=2.5)
image.plot(baseline_ratio, col=coolwarm(64), zlim=c(0.99,1.01), xlab = NA, ylab = NA, xaxt='n', yaxt='n')

png(paste0('/datasets/work/d61-af-soilmoisture/work/himawari/figures/directionality_2016-2020.png'), width=3200, height=2100)

layout(rbind(c(1,2,3,4),c(5,6,7,8),c(9,10,11,12)))
par(mar = c(0.2, 0.2, 0.2, 0.2), cex = 4)

image(baseline_par_a, col=spectralRamp(64), zlim=c(-0.04,0.04), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(baseline_par_a[], na.rm=TRUE), 3)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', format(median_value, nsmall=3)), bty='n')
legend('topleft', legend=paste0('(', letters[1], ')'), bty='n')

image(chiba_par_a, col=spectralRamp(64), zlim=c(-0.04,0.04), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(chiba_par_a[], na.rm=TRUE), 3)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', format(median_value, nsmall=3)), bty='n')
legend('topleft', legend=paste0('(', letters[2], ')'), bty='n')

image(copernicus_par_a, col=spectralRamp(64), zlim=c(-0.04,0.04), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(copernicus_par_a[], na.rm=TRUE), 3)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', format(median_value, nsmall=3)), bty='n')
legend('topleft', legend=paste0('(', letters[3], ')'), bty='n')

image(anu_calib_par_a, col=spectralRamp(64), zlim=c(-0.04,0.04), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anu_calib_par_a[], na.rm=TRUE), 3)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', format(median_value, nsmall=3)), bty='n')
legend('topleft', legend=paste0('(', letters[4], ')'), bty='n')

image(baseline_par_d, col=spectralRamp(64), zlim=c(-0.04,0.04), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(baseline_par_d[], na.rm=TRUE), 3)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', format(median_value, nsmall=3)), bty='n')
legend('topleft', legend=paste0('(', letters[5], ')'), bty='n')

image(chiba_par_d, col=spectralRamp(64), zlim=c(-0.04,0.04), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(chiba_par_d[], na.rm=TRUE), 3)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', format(median_value, nsmall=3)), bty='n')
legend('topleft', legend=paste0('(', letters[6], ')'), bty='n')

image(copernicus_par_d, col=spectralRamp(64), zlim=c(-0.04,0.04), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(copernicus_par_d[], na.rm=TRUE), 3)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', format(median_value, nsmall=3)), bty='n')
legend('topleft', legend=paste0('(', letters[7], ')'), bty='n')

image(anu_calib_par_d, col=spectralRamp(64), zlim=c(-0.04,0.04), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anu_calib_par_d[], na.rm=TRUE), 3)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', format(median_value, nsmall=3)), bty='n')
legend('topleft', legend=paste0('(', letters[8], ')'), bty='n')

image(baseline_ratio, col=coolwarm(64), zlim=c(0.99,1.01), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(baseline_ratio[], na.rm=TRUE), 3)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', format(median_value, nsmall=3)), bty='n')
legend('topleft', legend=paste0('(', letters[9], ')'), bty='n')

image(chiba_ratio, col=coolwarm(64), zlim=c(0.99,1.01), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(chiba_ratio[], na.rm=TRUE), 3)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', format(median_value, nsmall=3)), bty='n')
legend('topleft', legend=paste0('(', letters[10], ')'), bty='n')

image(copernicus_ratio, col=coolwarm(64), zlim=c(0.99,1.01), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(copernicus_ratio[], na.rm=TRUE), 3)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', format(median_value, nsmall=3)), bty='n')
legend('topleft', legend=paste0('(', letters[11], ')'), bty='n')

image(anu_calib_ratio, col=coolwarm(64), zlim=c(0.99,1.01), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anu_calib_ratio[], na.rm=TRUE), 3)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', format(median_value, nsmall=3)), bty='n')
legend('topleft', legend=paste0('(', letters[12], ')'), bty='n')

dev.off()

#########################################
# spatial distribution of Copernicus SZAC
#########################################

cop_szac_coeff = raster('/datasets/work/d61-af-soilmoisture/work/himawari/Copernicus_ZAC/Brent/SZAC_Copernicus_log_par1_2km_1to99percentile.tif')

# get SZAC value on a specific date
Dates = as.Date('2016-12-21')
sza_rst = raster(paste0('/datasets/work/d61-af-soilmoisture/work/SatelliteLST/Himawari_SZA/', format(Dates, '%Y/%m/%d/%Y%m%d0200_Himawari_SolarZenithAngle_AusSubset.tif')))
szac_rst = cop_szac_coeff * log(cos(sza_rst * pi/180) + 1)

cop_szac_coeff[cop_szac_coeff > 6] = 6; cop_szac_coeff[cop_szac_coeff < -2] = -2
#szac_rst[szac_rst > 8] = 8; szac_rst[szac_rst < 2] = 2

png('/datasets/work/d61-af-soilmoisture/work/himawari/figures/Copernicus_SZAC_plot.png', width=1600, height=700)
par(mar = c(0.2, 0.2, 0.2, 0.2), cex = 2.5)
m = cbind(1,2); layout(m)

image(cop_szac_coeff, col=vegeRamp(64), zlim=c(-2,6), xlab=NA, ylab=NA, , xaxt='n', yaxt='n')
addCoastLines(Proj=PROJ_LATLON, Colour='black')

image(szac_rst, col=TemperatureRamp(64), zlim=c(-1,5), xlab=NA, ylab=NA, xaxt='n', yaxt='n')
addCoastLines(Proj=PROJ_LATLON, Colour='black')

dev.off()

##################################
# plot Copernicus_SZAC LST metrics
##################################

# legend for the boxplot
library(ggsci)
SCICOL = pal_nejm('default', alpha=0.8)(6)

png('/datasets/work/d61-af-soilmoisture/work/himawari/figures/boxplot_legend.png', width=1500, height=800)

plot(1:10, 1:10, col='white')
legend(4.4,2, legend = 'Copernicus', pch=15, col=SCICOL[3], bty='n', cex=3)
legend(6.8,2, legend = 'Copernicus_SZAC', pch=15, col=SCICOL[6], bty='n', cex=3)

dev.off()

## draw the figure
library(ggsci)
SCICOL = pal_nejm('default', alpha=0.7)(6)

library(stringr)

ozfluxLL = read.csv('/datasets/work/d61-af-soilmoisture/work/himawari/in_situ_evaluations/emis_lookup.csv')

# drop HowardSprings and Litchfield
dropped_rows = which(ozfluxLL$sitename == 'HowardSprings' | ozfluxLL$sitename == 'Litchfield')
ozfluxLL = ozfluxLL[-dropped_rows,]

path1 = '/datasets/work/d61-af-soilmoisture/work/himawari/in_situ_evaluations/anu_baseline_for_fitting_updated/'
path2 = '/datasets/work/d61-af-soilmoisture/work/himawari/in_situ_evaluations/copernicus/'
path3 = '/datasets/work/d61-af-soilmoisture/work/himawari/in_situ_evaluations/copernicus_szac/'

### ***** This is the boxplot for stats of bias and ubRMSE *****

bias_ls_cop = c(); bias_ls_cop_szac = c()
ubRMSE_ls_cop = c(); ubRMSE_ls_cop_szac = c()
sample_number_total = c()

for (k in 1:20){
    
    data1 = read.csv(paste0(path1, ozfluxLL$sitename[k], '_extracted_data.csv'))
    data2 = read.csv(paste0(path2, ozfluxLL$sitename[k], '_extracted_data.csv'))
    data3 = read.csv(paste0(path3, ozfluxLL$sitename[k], '_extracted_data.csv'))
    
    # combine different dataframes
    df = cbind(data1, data2[,'copernicus'], data3[,'copernicus_szac'])
    df = na.omit(df); df = with(df, df[solar_angle <= 85, ]) # only use daytime
    colnames(df)[6:7] = c('copernicus', 'copernicus_szac')

    bias_cop = mean(df$copernicus - df$flux, na.rm=TRUE); bias_ls_cop = c(bias_ls_cop, bias_cop)
    bias_cop_szac = mean(df$copernicus_szac - df$flux, na.rm=TRUE); bias_ls_cop_szac = c(bias_ls_cop_szac, bias_cop_szac)

    ubRMSE_cop = sd(df$copernicus - df$flux, na.rm=TRUE); ubRMSE_ls_cop = c(ubRMSE_ls_cop, ubRMSE_cop)
    ubRMSE_cop_szac = sd(df$copernicus_szac - df$flux, na.rm=TRUE); ubRMSE_ls_cop_szac = c(ubRMSE_ls_cop_szac, ubRMSE_cop_szac)

    sample_number = nrow(df); sample_number_total = c(sample_number_total, sample_number)
}

N_total = sum(sample_number_total)

bias_cop_mean = sum(bias_ls_cop * sample_number_total) / N_total
bias_cop_szac_mean = sum(bias_ls_cop_szac * sample_number_total) / N_total

ubRMSE_cop_mean = sum(ubRMSE_ls_cop * sample_number_total) / N_total
ubRMSE_cop_szac_mean = sum(ubRMSE_ls_cop_szac * sample_number_total) / N_total

png('/datasets/work/d61-af-soilmoisture/work/himawari/figures/Cop_SZAC_in_situ_daytime_metrics.png', width=1600, height=600)

m = cbind(1,2)
layout(m); par(mar = c(2, 4, 2, 2))

boxplot(data.frame(bias_ls_cop, bias_ls_cop_szac), col=SCICOL[c(3,6)], xaxt='n', ylim=c(-8, 8), cex.axis=3); abline(a=0,b=0, lty='dashed')
#legend('topleft', legend=paste0('(b) Sample number = ', formatC(N_total, big.mark=',')), bty='n', cex=3)
points(1:2, c(bias_cop_mean, bias_cop_szac_mean), col='red', pch=19, cex=3)
text(1:2, -8, labels = format(round(c(bias_cop_mean, bias_cop_szac_mean), 2), nsmall = 2), col = 'red', cex = 3)

boxplot(data.frame(ubRMSE_ls_cop, ubRMSE_ls_cop_szac), col=SCICOL[c(3,6)], xaxt='n', ylim=c(1,5), cex.axis=3)
#legend('topleft', legend=paste0('(c) Sample number = ', formatC(N_total, big.mark=',')), bty='n', cex=3)
points(1:2, c(ubRMSE_cop_mean, ubRMSE_cop_szac_mean), col='red', pch=19, cex=3)
text(1:2, 1, labels = format(round(c(ubRMSE_cop_mean, ubRMSE_cop_szac_mean), 2), nsmall = 2), col = 'red', cex = 3)

dev.off()

### ***** This is the hourly boxplots for the bias *****

# nrow: site number; ncol: hour
global_df_cop = data.frame(matrix(nrow=20,ncol=24))
colnames(global_df_cop) = 0:23
global_df_cop_szac = global_df_cop

TOIs =  seq(ISOdatetime(2016,1,1,1,0,0,tz='GMT'),ISOdatetime(2020,12,31,23,0,0,tz='GMT'),3600)

for (k in 1:nrow(ozfluxLL)){

    data1 = read.csv(paste0(path1, ozfluxLL$sitename[k], '_extracted_data.csv'))
    data2 = read.csv(paste0(path2, ozfluxLL$sitename[k], '_extracted_data.csv'))
    data3 = read.csv(paste0(path3, ozfluxLL$sitename[k], '_extracted_data.csv'))

    # combine different dataframes
    df = cbind(data1, data2[,'copernicus'], data3[,'copernicus_szac'])
    colnames(df)[6:7] = c('copernicus', 'copernicus_szac')

    # need formatted time
    standard_time = TOIs; attr(standard_time, 'tzone') = ozfluxLL$standard_timezone[k]
    df$local_time = standard_time
    df = na.omit(df)

    # calc the hourly difference for everyday
    df_diff_cop = data.frame(local_time = as.numeric(format(df$local_time, '%H')),
                            diff = df$copernicus - df$flux)
    df_diff_cop_szac = data.frame(local_time = as.numeric(format(df$local_time, '%H')),
                            diff = df$copernicus_szac - df$flux)

    # group by hour for the period of 2016-2020
    for (t in 0:23){

        select_df_cop = with(df_diff_cop, df_diff_cop[local_time == t,])
        global_df_cop[k,t+1] = mean(select_df_cop$diff)

        select_df_cop_szac = with(df_diff_cop_szac, df_diff_cop_szac[local_time == t,])
        global_df_cop_szac[k,t+1] = mean(select_df_cop_szac$diff)
    }
}

## ***** Here to plot all four data *****
## https://stackoverflow.com/questions/14604439/plot-multiple-boxplot-in-one-graph

library(ggsci)
SCICOL = pal_nejm('default', alpha=0.7)(6)

png('/datasets/work/d61-af-soilmoisture/work/himawari/figures/Cop_SZAC_in_situ_daytime_hourly_bias.png', width = 1600, height = 600)

boxplot(global_df_cop[,6:20], cex.axis = 2.5, boxfill=SCICOL[3], ylim=c(-8,12), 
        boxwex=0.25, at = 0:14 - 0.2) # shift these left by -0.2

boxplot(global_df_cop_szac[,6:20], xaxt = 'n', yaxt = 'n', add = TRUE, boxfill=SCICOL[6], 
        boxwex=0.25, at = 0:14) # position unchanged

abline(h=0, col='black', lty=2, lwd = 2)

dev.off()
