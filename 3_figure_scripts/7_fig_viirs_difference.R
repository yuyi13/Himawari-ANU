# ################################################################
# objective:   generate plots for comparison against viirs
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_SetupForGraphics.R')
library(pals); library(fields)

path2viirs = '/g/data/dt1/SatelliteLST/'

tem = raster(extent(112,154,-45,-10), res=0.01, crs=PROJ_LATLON)
DOI = as.POSIXct('2019-12-20 04:00', tz='GMT') # GMT time, equals to 2019-12-20 14:00 AEST

anuv1.0 = raster(paste0('/g/data/dt1/SatelliteLST/H8_LST_ANU_baseline/', format(DOI, '%Y/%m/%d/%Y%m%d%H%M'), '00_AHI_ANU_LSTv1.0_AusSubset.tif'))
chiba = raster(paste0('/g/data/dt1/SatelliteLST/H8_LST_Chiba/', format(DOI, '%Y/%m/%d/%Y%m%d%H%M'), '_AHI_Chiba_LSTv0_AusSubset.tif'))
copernicus = raster(paste0('/g/data/dt1/SatelliteLST/Copernicus_LST_AUS/', format(DOI, '%Y/%m/%d/%Y%m%d%H%M'), '_C_GLS_GEO_LSTv1.2.1_AusSubset.tiff'))
anuv1.4 = raster(paste0('/g/data/lr26/Himawari-AHI_LST_ANU/LST/', format(DOI, '%Y/%m/%d/%Y%m%d%H%M'), '00_AHI_ANU_LSTv1.4.1_AusSubset.tif'))

anuv1.0 = mask(anuv1.0, chiba)
anuv1.4 = mask(anuv1.4, chiba)
copernicus = copernicus * 0.01 + 273.15
copernicus = projectRaster(copernicus, anuv1.0, method='ngb')

anuv1.0[anuv1.0 > 340] = 340; anuv1.0[anuv1.0 < 280] = 280
chiba[chiba > 340] = 340; chiba[chiba < 280] = 280
copernicus[copernicus > 340] = 340; copernicus[copernicus < 280] = 280
anuv1.4[anuv1.4 > 340] = 340; anuv1.4[anuv1.4 < 280] = 280

vii = raster(paste0('/g/data/dt1/SatelliteLST/VIIRS_LST/LST/', format(DOI, '%Y/%Y%m%d_VNP21A1D_LST_AusSubset.tif')))
vii = projectRaster(vii, anuv1.0, method='ngb')

viitime = raster(paste0('/g/data/dt1/SatelliteLST/VIIRS_LST/Time/', format(DOI, '%Y/%Y%m%d_VNP21A1D_Time_AusSubset.tif')))
viitime = projectRaster(viitime, anuv1.0, method='ngb')

lon_mtx = matrix(NA, nrow=1750, ncol=2100)
for (k in 1:ncol(lon_mtx)){
    lon_mtx[,k] = 112.01 + 0.02 * (k-1)
}
lon_rst = raster(lon_mtx, xmn=112, xmx=154, ymn=-45, ymx=-10, crs=PROJ_LATLON)

# give a one-hour temporal window of 4:00 GMT to match Himawari
viitime = viitime - lon_rst/15; viitime[viitime < 3.5 | viitime > 4.5] = NA
vii = mask(vii, viitime)

# draw the figure

png(paste0('/g/data/os22/users/yu/himawari/figures/viirs_spat_compar_', format(DOI, '%Y%m%d'), '.png'), width=1500, height=3000)

layout(rbind(c(1,2),c(3,4),c(5,6),c(7,8),c(9,10)), widths = c(1,1,1,1), heights = c(1,1,1,1))
par(mar = c(0.5, 0.5, 0.5, 0.5))

image(anuv1.0, col=TemperatureRamp(64), zlim=c(280,340), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = 'Baseline', cex=6, bty='n')
anu_diff = anuv1.0 - vii; anu_diff[anu_diff > 10] = 10; anu_diff[anu_diff < -10] = -10
image(anu_diff, col = coolwarm(64), zlim=c(-10,10), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = expression('LST'['diff']*' (Baseline - VIIRS)'), cex=6, bty='n')

image(chiba, col=TemperatureRamp(64), zlim=c(280,340), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = 'Chiba', cex=6, bty='n')
chiba_diff = chiba - vii; chiba_diff[chiba_diff > 10] = 10; chiba_diff[chiba_diff < -10] = -10
image(chiba_diff, col = coolwarm(64), zlim=c(-10,10), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = expression('LST'['diff']*' (Chiba - VIIRS)'), cex=6, bty='n')

image(copernicus, col=TemperatureRamp(64), zlim=c(280,340), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = 'Copernicus', cex=6, bty='n')
copernicus_diff = copernicus - vii; copernicus_diff[copernicus_diff > 10] = 10; copernicus_diff[copernicus_diff < -10] = -10
image(copernicus_diff, col = coolwarm(64), zlim=c(-10,10), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = expression('LST'['diff']*' (Copernicus - VIIRS)'), cex=6, bty='n')

image(anuv1.4, col=TemperatureRamp(64), zlim=c(280,340), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = expression('ANU'['SZAC']), cex=6, bty='n')
anuv1.4_diff = anuv1.4 - vii; anuv1.4_diff[anuv1.4_diff > 10] = 10; anuv1.4_diff[anuv1.4_diff < -10] = -10
image(anuv1.4_diff, col = coolwarm(64), zlim=c(-10,10), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = expression('LST'['diff']*' (ANU'['SZAC']*' - VIIRS)'), , cex=6, bty='n')

image(vii, col=TemperatureRamp(64), zlim=c(280,340), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = 'VIIRS', cex=6, bty='n')

image(viitime, col=rainbow(64), zlim=c(3.5,4.5), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
addCoastLines(Proj=PROJ_LATLON, Colour='black'); legend('bottomleft', legend = 'VIIRS Time', cex=6, bty='n')

dev.off()

# make a legend
comment = 
"
par(cex=2.5)
image.plot(anuv1.4, col=TemperatureRamp(64), zlim=c(280,340), xlab = NA, ylab = NA, xaxt='n', yaxt='n')

par(cex=2.5)
image.plot(viitime, col=rainbow(64), zlim=c(3.5,4.5), xlab = NA, ylab = NA, xaxt='n', yaxt='n')

par(cex=2.5)
image.plot(anuv1.4_diff, col = coolwarm(64), zlim=c(-10,10), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
"