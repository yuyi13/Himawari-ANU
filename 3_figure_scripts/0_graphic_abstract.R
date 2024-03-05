# ################################################################
# objective:   generate graphic abstract
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_SetupForGraphics.R')
library(pals); library(fields)

path2metrics = '/datasets/work/d61-af-soilmoisture/work/himawari/pixelwise_metrics/'

anu_terra_bias = raster(paste0(path2metrics, 'ANU_minus_MOD11A1_bias_2016_2020.tif'))
anucalib_terra_bias = raster(paste0(path2metrics, 'ANUcalib_minus_MOD11A1_bias_2016_2020.tif'))

anu_aqua_bias = raster(paste0(path2metrics, 'ANU_minus_MYD11A1_bias_2016_2020.tif'))
anucalib_aqua_bias = raster(paste0(path2metrics, 'ANUcalib_minus_MYD11A1_bias_2016_2020.tif'))

anu_viirs_bias = raster(paste0(path2metrics, 'ANU_minus_VIIRS_bias_2016_2020.tif'))
anucalib_viirs_bias = raster(paste0(path2metrics, 'ANUcalib_minus_VIIRS_bias_2016_2020.tif'))

# make a legend for bias and ubRMSE
par(cex=1.8)
image.plot(anu_terra_bias, col=coolwarm(64), zlim=c(-8,8), xlab = NA, ylab = NA, xaxt='n', yaxt='n')

png(paste0('/datasets/work/d61-af-soilmoisture/work/himawari/figures/graphic_abs1.png'), width=500, height=1200)

layout(cbind(c(1,2,3)), widths = c(1,1,1,1), heights = c(1,1,1,1))
par(mar = c(0.5, 0.5, 0.5, 0.5))

image(anu_terra_bias, col=coolwarm(64), zlim=c(-8,8), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anu_terra_bias[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K') , cex=5, bty='n')

image(anu_aqua_bias, col=coolwarm(64), zlim=c(-8,8), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anu_aqua_bias[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K'), cex=5, bty='n')

image(anu_viirs_bias, col=coolwarm(64), zlim=c(-8,8), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anu_viirs_bias[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K'), cex=5, bty='n')

dev.off()


png(paste0('/datasets/work/d61-af-soilmoisture/work/himawari/figures/graphic_abs2.png'), width=500, height=1200)

layout(cbind(c(1,2,3)), widths = c(1,1,1,1), heights = c(1,1,1,1))
par(mar = c(0.5, 0.5, 0.5, 0.5))

image(anucalib_terra_bias, col=coolwarm(64), zlim=c(-8,8), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anucalib_terra_bias[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K'), cex=5, bty='n')

image(anucalib_aqua_bias, col=coolwarm(64), zlim=c(-8,8), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anucalib_aqua_bias[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K'), cex=5, bty='n')

image(anucalib_viirs_bias, col=coolwarm(64), zlim=c(-8,8), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
median_value = round(median(anucalib_viirs_bias[], na.rm=TRUE), 2)
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = paste0('Median = ', median_value, ' K'), cex=5, bty='n')

dev.off()
