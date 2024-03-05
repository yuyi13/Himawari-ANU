# ################################################################
# objective:   generate plots of metrics against modis lst
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_SetupForGraphics.R')
library(pals); library(fields)

path2metrics = '/datasets/work/d61-af-soilmoisture/work/himawari/pixelwise_metrics/'

####################################
# plot metrics for MODIS best pixels
####################################

anu_bias = raster(paste0(path2metrics, 'ANU_minus_MODISbest_bias_2016_2020.tif'))
anu_ubRMSE = raster(paste0(path2metrics, 'ANU_minus_MODISbest_ubRMSE_2016_2020.tif'))
anu_valid_pixels = raster(paste0(path2metrics, 'ANU_minus_MODISbest_valid_pixels_2016_2020.tif'))
anu_valid_pixels[anu_valid_pixels == 0] = NA

anucalib_bias = raster(paste0(path2metrics, 'ANUcalib_minus_MODISbest_bias_2016_2020.tif'))
anucalib_ubRMSE = raster(paste0(path2metrics, 'ANUcalib_minus_MODISbest_ubRMSE_2016_2020.tif'))
anucalib_valid_pixels = raster(paste0(path2metrics, 'ANUcalib_minus_MODISbest_valid_pixels_2016_2020.tif'))
anucalib_valid_pixels[anucalib_valid_pixels == 0] = NA

# make a legend for bias, ubRMSE and valid pixels
par(cex=2.5)
image.plot(anu_bias, col=coolwarm(64), zlim=c(-8,8), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
par(cex=2.5)
image.plot(anu_ubRMSE, col=TemperatureRamp(64), zlim=c(0,5), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
par(cex=2.5)
image.plot(anu_valid_pixels, col=viridis(64), zlim=c(0,2500), xlab = NA, ylab = NA, xaxt='n', yaxt='n')

png(paste0('/datasets/work/d61-af-soilmoisture/work/himawari/figures/MODIS_fitting_2016-2020.png'), width=2000, height=1200)

layout(rbind(c(1,2,3),c(4,5,6)))
par(mar = c(0.2, 0.2, 0.2, 0.2), cex = 3)

image(anu_bias, col=coolwarm(64), zlim=c(-8,8), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anu_bias[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K') , bty='n')
legend('topleft', legend=paste0('(', letters[1], ')'), bty='n')

image(anu_ubRMSE, col=TemperatureRamp(64), zlim=c(0,5), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anu_ubRMSE[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K') , bty='n')
legend('topleft', legend=paste0('(', letters[2], ')'), bty='n')

image(anu_valid_pixels, col=viridis(64), zlim=c(0,2500), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anu_valid_pixels[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median number = ', median_value) , bty='n')
legend('topleft', legend=paste0('(', letters[3], ')'), bty='n')

image(anucalib_bias, col=coolwarm(64), zlim=c(-8,8), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anucalib_bias[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K') , bty='n')
legend('topleft', legend=paste0('(', letters[4], ')'), bty='n')

image(anucalib_ubRMSE, col=TemperatureRamp(64), zlim=c(0,5), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anucalib_ubRMSE[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K') , bty='n')
legend('topleft', legend=paste0('(', letters[5], ')'), bty='n')

image(anucalib_valid_pixels, col=viridis(64), zlim=c(0,2500), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anucalib_valid_pixels[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median number = ', median_value) , bty='n')
legend('topleft', legend=paste0('(', letters[6], ')'), bty='n')

dev.off()

##############################
# plot metrics for MODIS/Terra
##############################

anu_terra_bias = raster(paste0(path2metrics, 'ANU_minus_MOD11A1_bias_2016_2020.tif'))
anucalib_terra_bias = raster(paste0(path2metrics, 'ANUcalib_minus_MOD11A1_bias_2016_2020.tif'))
chiba_terra_bias = raster(paste0(path2metrics, 'Chiba_minus_MOD11A1_bias_2016_2020.tif'))
copernicus_terra_bias = raster(paste0(path2metrics, 'Copernicus_minus_MOD11A1_bias_2016_2020.tif'))

anu_terra_ubRMSE = raster(paste0(path2metrics, 'ANU_minus_MOD11A1_ubRMSE_2016_2020.tif'))
anucalib_terra_ubRMSE = raster(paste0(path2metrics, 'ANUcalib_minus_MOD11A1_ubRMSE_2016_2020.tif'))
chiba_terra_ubRMSE = raster(paste0(path2metrics, 'Chiba_minus_MOD11A1_ubRMSE_2016_2020.tif'))
copernicus_terra_ubRMSE = raster(paste0(path2metrics, 'Copernicus_minus_MOD11A1_ubRMSE_2016_2020.tif'))

anu_valid_pixels = raster(paste0(path2metrics, 'ANU_minus_MOD11A1_valid_pixels_2016_2020.tif'))
anu_valid_pixels[anu_valid_pixels == 0] = NA
anucalib_valid_pixels = raster(paste0(path2metrics, 'ANUcalib_minus_MOD11A1_valid_pixels_2016_2020.tif'))
anucalib_valid_pixels[anucalib_valid_pixels == 0] = NA
chiba_valid_pixels = raster(paste0(path2metrics, 'Chiba_minus_MOD11A1_valid_pixels_2016_2020.tif'))
chiba_valid_pixels[chiba_valid_pixels == 0] = NA
copernicus_valid_pixels = raster(paste0(path2metrics, 'Copernicus_minus_MOD11A1_valid_pixels_2016_2020.tif'))
copernicus_valid_pixels[copernicus_valid_pixels == 0] = NA

png(paste0('/datasets/work/d61-af-soilmoisture/work/himawari/figures/Terra_validation_2016-2020.png'), width=3200, height=2100)

layout(rbind(c(1,2,3,4),c(5,6,7,8),c(9,10,11,12)))
par(mar = c(0.2, 0.2, 0.2, 0.2), cex = 4)

image(anu_terra_bias, col=coolwarm(64), zlim=c(-8,8), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anu_terra_bias[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K') , bty='n')
legend('topleft', legend=paste0('(', letters[1], ')'), bty='n')

image(chiba_terra_bias, col=coolwarm(64), zlim=c(-8,8), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(chiba_terra_bias[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K') , bty='n')
legend('topleft', legend=paste0('(', letters[2], ')'), bty='n')

image(copernicus_terra_bias, col=coolwarm(64), zlim=c(-8,8), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(copernicus_terra_bias[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K') , bty='n')
legend('topleft', legend=paste0('(', letters[3], ')'), bty='n')

image(anucalib_terra_bias, col=coolwarm(64), zlim=c(-8,8), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anucalib_terra_bias[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K') , bty='n')
legend('topleft', legend=paste0('(', letters[4], ')'), bty='n')

image(anu_terra_ubRMSE, col=TemperatureRamp(64), zlim=c(0,5), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anu_terra_ubRMSE[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K') , bty='n')
legend('topleft', legend=paste0('(', letters[5], ')'), bty='n')

image(chiba_terra_ubRMSE, col=TemperatureRamp(64), zlim=c(0,5), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(chiba_terra_ubRMSE[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K') , bty='n')
legend('topleft', legend=paste0('(', letters[6], ')'), bty='n')

image(copernicus_terra_ubRMSE, col=TemperatureRamp(64), zlim=c(0,5), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(copernicus_terra_ubRMSE[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K') , bty='n')
legend('topleft', legend=paste0('(', letters[7], ')'), bty='n')

image(anucalib_terra_ubRMSE, col=TemperatureRamp(64), zlim=c(0,5), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anucalib_terra_ubRMSE[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K') , bty='n')
legend('topleft', legend=paste0('(', letters[8], ')'), bty='n')

image(anu_valid_pixels, col=viridis(64), zlim=c(0,1500), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anu_valid_pixels[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend=paste0('Median number = ', median_value), bty='n')
legend('topleft', legend=paste0('(', letters[9], ')'), bty='n')

image(chiba_valid_pixels, col=viridis(64), zlim=c(0,1500), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(chiba_valid_pixels[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend=paste0('Median number = ', median_value), bty='n')
legend('topleft', legend=paste0('(', letters[10], ')'), bty='n')

image(copernicus_valid_pixels, col=viridis(64), zlim=c(0,1500), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(copernicus_valid_pixels[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend=paste0('Median number = ', median_value), bty='n')
legend('topleft', legend=paste0('(', letters[11], ')'), bty='n')

image(anucalib_valid_pixels, col=viridis(64), zlim=c(0,1500), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anucalib_valid_pixels[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend=paste0('Median number = ', median_value), bty='n')
legend('topleft', legend=paste0('(', letters[12], ')'), bty='n')

dev.off()

#############################
# plot metrics for MODIS/Aqua
#############################

anu_aqua_bias = raster(paste0(path2metrics, 'ANU_minus_MYD11A1_bias_2016_2020.tif'))
anucalib_aqua_bias = raster(paste0(path2metrics, 'ANUcalib_minus_MYD11A1_bias_2016_2020.tif'))
chiba_aqua_bias = raster(paste0(path2metrics, 'Chiba_minus_MYD11A1_bias_2016_2020.tif'))
copernicus_aqua_bias = raster(paste0(path2metrics, 'Copernicus_minus_MYD11A1_bias_2016_2020.tif'))

anu_aqua_ubRMSE = raster(paste0(path2metrics, 'ANU_minus_MYD11A1_ubRMSE_2016_2020.tif'))
anucalib_aqua_ubRMSE = raster(paste0(path2metrics, 'ANUcalib_minus_MYD11A1_ubRMSE_2016_2020.tif'))
chiba_aqua_ubRMSE = raster(paste0(path2metrics, 'Chiba_minus_MYD11A1_ubRMSE_2016_2020.tif'))
copernicus_aqua_ubRMSE = raster(paste0(path2metrics, 'Copernicus_minus_MYD11A1_ubRMSE_2016_2020.tif'))

anu_valid_pixels = raster(paste0(path2metrics, 'ANU_minus_MYD11A1_valid_pixels_2016_2020.tif'))
anu_valid_pixels[anu_valid_pixels == 0] = NA
anucalib_valid_pixels = raster(paste0(path2metrics, 'ANUcalib_minus_MYD11A1_valid_pixels_2016_2020.tif'))
anucalib_valid_pixels[anucalib_valid_pixels == 0] = NA
chiba_valid_pixels = raster(paste0(path2metrics, 'Chiba_minus_MYD11A1_valid_pixels_2016_2020.tif'))
chiba_valid_pixels[chiba_valid_pixels == 0] = NA
copernicus_valid_pixels = raster(paste0(path2metrics, 'Copernicus_minus_MYD11A1_valid_pixels_2016_2020.tif'))
copernicus_valid_pixels[copernicus_valid_pixels == 0] = NA

png(paste0('/datasets/work/d61-af-soilmoisture/work/himawari/figures/Aqua_validation_2016-2020.png'), width=3200, height=2100)

layout(rbind(c(1,2,3,4),c(5,6,7,8),c(9,10,11,12)))
par(mar = c(0.2, 0.2, 0.2, 0.2), cex = 4)

image(anu_aqua_bias, col=coolwarm(64), zlim=c(-8,8), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anu_aqua_bias[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K'), bty='n')
legend('topleft', legend=paste0('(', letters[1], ')'), bty='n')

image(chiba_aqua_bias, col=coolwarm(64), zlim=c(-8,8), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(chiba_aqua_bias[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K'), bty='n')
legend('topleft', legend=paste0('(', letters[2], ')'), bty='n')

image(copernicus_aqua_bias, col=coolwarm(64), zlim=c(-8,8), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(copernicus_aqua_bias[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K'), bty='n')
legend('topleft', legend=paste0('(', letters[3], ')'), bty='n')

image(anucalib_aqua_bias, col=coolwarm(64), zlim=c(-8,8), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anucalib_aqua_bias[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K'), bty='n')
legend('topleft', legend=paste0('(', letters[4], ')'), bty='n')

image(anu_aqua_ubRMSE, col=TemperatureRamp(64), zlim=c(0,5), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anu_aqua_ubRMSE[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K') , bty='n')
legend('topleft', legend=paste0('(', letters[5], ')'), bty='n')

image(chiba_aqua_ubRMSE, col=TemperatureRamp(64), zlim=c(0,5), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(chiba_aqua_ubRMSE[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K') , bty='n')
legend('topleft', legend=paste0('(', letters[6], ')'), bty='n')

image(copernicus_aqua_ubRMSE, col=TemperatureRamp(64), zlim=c(0,5), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(copernicus_aqua_ubRMSE[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K') , bty='n')
legend('topleft', legend=paste0('(', letters[7], ')'), bty='n')

image(anucalib_aqua_ubRMSE, col=TemperatureRamp(64), zlim=c(0,5), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anucalib_aqua_ubRMSE[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K') , bty='n')
legend('topleft', legend=paste0('(', letters[8], ')'), bty='n')

image(anu_valid_pixels, col=viridis(64), zlim=c(0,1500), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anu_valid_pixels[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend=paste0('Median number = ', median_value), bty='n')
legend('topleft', legend=paste0('(', letters[9], ')'), bty='n')

image(chiba_valid_pixels, col=viridis(64), zlim=c(0,1500), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(chiba_valid_pixels[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend=paste0('Median number = ', median_value), bty='n')
legend('topleft', legend=paste0('(', letters[10], ')'), bty='n')

image(copernicus_valid_pixels, col=viridis(64), zlim=c(0,1500), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(copernicus_valid_pixels[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend=paste0('Median number = ', median_value), bty='n')
legend('topleft', legend=paste0('(', letters[11], ')'), bty='n')

image(anucalib_valid_pixels, col=viridis(64), zlim=c(0,1500), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anucalib_valid_pixels[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend=paste0('Median number = ', median_value), bty='n')
legend('topleft', legend=paste0('(', letters[12], ')'), bty='n')

dev.off()

########################
# plot metrics for VIIRS
########################

anu_viirs_bias = raster(paste0(path2metrics, 'ANU_minus_VIIRS_bias_2016_2020.tif'))
anucalib_viirs_bias = raster(paste0(path2metrics, 'ANUcalib_minus_VIIRS_bias_2016_2020.tif'))
chiba_viirs_bias = raster(paste0(path2metrics, 'Chiba_minus_VIIRS_bias_2016_2020.tif'))
copernicus_viirs_bias = raster(paste0(path2metrics, 'Copernicus_minus_VIIRS_bias_2016_2020.tif'))

anu_viirs_bias[anu_viirs_bias < -4] = -4; anu_viirs_bias[anu_viirs_bias > 8] = 8
anucalib_viirs_bias[anucalib_viirs_bias < -4] = -4; anucalib_viirs_bias[anucalib_viirs_bias > 8] = 8
chiba_viirs_bias[chiba_viirs_bias < -4] = -4; chiba_viirs_bias[chiba_viirs_bias > 8] = 8
copernicus_viirs_bias[copernicus_viirs_bias < -4] = -4; copernicus_viirs_bias[copernicus_viirs_bias > 8] = 8

anu_viirs_ubRMSE = raster(paste0(path2metrics, 'ANU_minus_VIIRS_ubRMSE_2016_2020.tif'))
anucalib_viirs_ubRMSE = raster(paste0(path2metrics, 'ANUcalib_minus_VIIRS_ubRMSE_2016_2020.tif'))
chiba_viirs_ubRMSE = raster(paste0(path2metrics, 'Chiba_minus_VIIRS_ubRMSE_2016_2020.tif'))
copernicus_viirs_ubRMSE = raster(paste0(path2metrics, 'Copernicus_minus_VIIRS_ubRMSE_2016_2020.tif'))

anu_valid_pixels = raster(paste0(path2metrics, 'ANU_minus_VIIRS_valid_pixels_2016_2020.tif'))
anu_valid_pixels[anu_valid_pixels == 0] = NA
anucalib_valid_pixels = raster(paste0(path2metrics, 'ANUcalib_minus_VIIRS_valid_pixels_2016_2020.tif'))
anucalib_valid_pixels[anucalib_valid_pixels == 0] = NA
chiba_valid_pixels = raster(paste0(path2metrics, 'Chiba_minus_VIIRS_valid_pixels_2016_2020.tif'))
chiba_valid_pixels[chiba_valid_pixels == 0] = NA
copernicus_valid_pixels = raster(paste0(path2metrics, 'Copernicus_minus_VIIRS_valid_pixels_2016_2020.tif'))
copernicus_valid_pixels[copernicus_valid_pixels == 0] = NA

png(paste0('/datasets/work/d61-af-soilmoisture/work/himawari/figures/VIIRS_validation_2016-2020.png'), width=3200, height=2100)

layout(rbind(c(1,2,3,4),c(5,6,7,8),c(9,10,11,12)))
par(mar = c(0.2, 0.2, 0.2, 0.2), cex = 4)

image(anu_viirs_bias, col=coolwarm(64), zlim=c(-8,8), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anu_viirs_bias[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K'), bty='n')
legend('topleft', legend=paste0('(', letters[1], ')'), bty='n')

image(chiba_viirs_bias, col=coolwarm(64), zlim=c(-8,8), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(chiba_viirs_bias[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K'), bty='n')
legend('topleft', legend=paste0('(', letters[2], ')'), bty='n')

image(copernicus_viirs_bias, col=coolwarm(64), zlim=c(-8,8), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(copernicus_viirs_bias[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K'), bty='n')
legend('topleft', legend=paste0('(', letters[3], ')'), bty='n')

image(anucalib_viirs_bias, col=coolwarm(64), zlim=c(-8,8), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anucalib_viirs_bias[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K'), bty='n')
legend('topleft', legend=paste0('(', letters[4], ')'), bty='n')

image(anu_viirs_ubRMSE, col=TemperatureRamp(64), zlim=c(0,5), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anu_viirs_ubRMSE[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K') , bty='n')
legend('topleft', legend=paste0('(', letters[5], ')'), bty='n')

image(chiba_viirs_ubRMSE, col=TemperatureRamp(64), zlim=c(0,5), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(chiba_viirs_ubRMSE[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K') , bty='n')
legend('topleft', legend=paste0('(', letters[6], ')'), bty='n')

image(copernicus_viirs_ubRMSE, col=TemperatureRamp(64), zlim=c(0,5), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(copernicus_viirs_ubRMSE[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K') , bty='n')
legend('topleft', legend=paste0('(', letters[7], ')'), bty='n')

image(anucalib_viirs_ubRMSE, col=TemperatureRamp(64), zlim=c(0,5), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anucalib_viirs_ubRMSE[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K') , bty='n')
legend('topleft', legend=paste0('(', letters[8], ')'), bty='n')

image(anu_valid_pixels, col=viridis(64), zlim=c(0,1500), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anu_valid_pixels[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend=paste0('Median number = ', median_value), bty='n')
legend('topleft', legend=paste0('(', letters[9], ')'), bty='n')

image(chiba_valid_pixels, col=viridis(64), zlim=c(0,1500), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(chiba_valid_pixels[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend=paste0('Median number = ', median_value), bty='n')
legend('topleft', legend=paste0('(', letters[10], ')'), bty='n')

image(copernicus_valid_pixels, col=viridis(64), zlim=c(0,1500), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(copernicus_valid_pixels[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend=paste0('Median number = ', median_value), bty='n')
legend('topleft', legend=paste0('(', letters[11], ')'), bty='n')

image(anucalib_valid_pixels, col=viridis(64), zlim=c(0,1500), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anucalib_valid_pixels[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend=paste0('Median number = ', median_value), bty='n')
legend('topleft', legend=paste0('(', letters[12], ')'), bty='n')

dev.off()
