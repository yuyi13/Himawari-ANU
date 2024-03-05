# ################################################################
# objective:   generate plots for szac and comparison against
# 			   vi and albedo data
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_SetupForGraphics.R')
library(RColorBrewer); library(viridis)
library(pals)
library(fields)

ozfluxLL = read.csv('/g/data/os22/users/yu/himawari/0_code/OzFluxSites_LatLon.csv')

# drop HowardSprings and Litchfield
dropped_rows = which(ozfluxLL$sitename == 'HowardSprings' | ozfluxLL$sitename == 'Litchfield')
ozfluxLL = ozfluxLL[-dropped_rows,]

ZAC_rst1 = raster('/g/data/fj4/himawari/ANCILLARY/ZAC/ZAC_v1.4.1_log_par1_2km_1to99percentile.tif')

SZA = raster('/g/data/ra22/satellite-products/arc/obs/himawari-ahi/fldk/latest/2016/01/01/0800/20160101080000-P1S-ABOM_GEOM_SOLAR-PRJ_GEOS141_2000-HIMAWARI8-AHI.nc',
			varname='solar_zenith_angle')
SZA_aus = projectRaster(SZA, ZAC_rst1, method='ngb')

ZAC_log = ZAC_rst1 * log(cos(SZA_aus * pi/180) + 1)

#############################
# spatial distribution of ZAC
#############################

png('/g/data/os22/users/yu/himawari/figures/ZAC_plot.png', width=1050, height=900)
par(cex=2.5, mar = c(2,2,2,4))
image(ZAC_rst1, col=vegeRamp(64), zlim=c(2,10), xlab=NA, ylab=NA)

for (k in 1:nrow(ozfluxLL)){
    points(ozfluxLL$lon[k], ozfluxLL$lat[k], pch=17, col='black', cex=1.2)
}

scalebar(1000, divs=4, type='bar', below='km')
addCoastLines(Proj=PROJ_LATLON, Colour='black')
abline(v=140.7, col='red', lwd=3, lty=2)
dev.off()

# legend
par(cex=1.5)
image.plot(ZAC_rst1, col=vegeRamp(64), zlim=c(2,10), xlab = NA, ylab = NA, xaxt='n', yaxt='n')

###########################
# seasonal variation of ZAC
###########################

path2emis = '/g/data/fj4/himawari/ANCILLARY/EMISS_SSEC/update2021/'
path2data = '/g/data/ra22/satellite-products/arc/obs/himawari-ahi/fldk/latest/'
path2lst_bas = '/g/data/dt1/SatelliteLST/H8_LST_ANU_baseline/'
path2lst_chiba = '/g/data/dt1/SatelliteLST/H8_LST_Chiba/'
path2lst_calib = '/g/data/lr26/Himawari-AHI_LST_ANU/LST/'
Dates = c(as.Date('2016-03-20'), as.Date('2016-06-21'), as.Date('2016-09-23'), as.Date('2016-12-21'))

png('/g/data/os22/users/yu/himawari/figures/ZAC_seasonal_variations.png', width=1800, height=1600)

layout(rbind(c(1,2,3,4), c(5,6,7,8), c(9,10,11,12), c(13,14,15,16)))
par(mar = c(0.2, 0.2, 0.2, 0.2))

for (t in 1:length(Dates)){

	print(Dates[t])

	emis_11um = raster(paste0(path2emis, '11um/global_emis_11um_monthly_2003-2016_', format(Dates[t], '%m.tif')))
	emis_12um = raster(paste0(path2emis, '12um/global_emis_12um_monthly_2003-2016_', format(Dates[t], '%m.tif')))

	emis_11um_aus = projectRaster(emis_11um, ZAC_rst1, method='ngb')
	emis_12um_aus = projectRaster(emis_12um, ZAC_rst1, method='ngb')

	# the utilised emissivity
	emis_averg = (emis_11um_aus + emis_12um_aus) / 2

	# solar zenith angle around midday
	SZA = raster(paste0(path2data, format(Dates[t], '%Y/%m/%d/0200/'), format(Dates[t], '%Y%m%d'), '020000-P1S-ABOM_GEOM_SOLAR-PRJ_GEOS141_2000-HIMAWARI8-AHI.nc'),
			varname='solar_zenith_angle')
	SZA_aus = projectRaster(SZA, ZAC_rst1, method='ngb')
	ZAC_log = ZAC_rst1 * log(cos(SZA_aus * pi/180) + 1)

	# mask
	chiba_mask = raster(paste0(path2lst_chiba, format(Dates[t], '%Y/%m/%d/'), format(Dates[t], '%Y%m%d'), '0200_AHI_Chiba_LSTv0_AusSubset.tif'))

	# baseline
	baseline = raster(paste0(path2lst_bas, format(Dates[t], '%Y/%m/%d/'), format(Dates[t], '%Y%m%d'), '020000_AHI_ANU_LSTv1.0_AusSubset.tif'))
	baseline = mask(baseline, chiba_mask)

	# anu_zac (or anu_calib)
	anu_zac = raster(paste0(path2lst_calib, format(Dates[t], '%Y/%m/%d/'), format(Dates[t], '%Y%m%d'), '020000_AHI_ANU_LSTv1.4.1_AusSubset.tif'))
	anu_zac = mask(anu_zac, chiba_mask)

	# finally plot the figures
	image(emis_averg, col=coolwarm(64), zlim=c(0.92,1), xlab=NA, ylab=NA, xaxt='n', yaxt='n')
	addCoastLines(Proj=PROJ_LATLON, Colour='black')
	legend('topleft', legend = paste0('(', letters[(t-1)*4+1], ')'), cex=4, bty='n')

	image(ZAC_log, col=TemperatureRamp(64), zlim=c(0,8), xlab=NA, ylab=NA, xaxt='n', yaxt='n')
	addCoastLines(Proj=PROJ_LATLON, Colour='black')
	legend('topleft', legend = paste0('(', letters[(t-1)*4+2], ')'), cex=4, bty='n')

	image(baseline, col=TemperatureRamp(64), zlim=c(280,340), xlab=NA, ylab=NA, xaxt='n', yaxt='n')
	addCoastLines(Proj=PROJ_LATLON, Colour='black')
	legend('topleft', legend = paste0('(', letters[(t-1)*4+3], ')'), cex=4, bty='n')

	image(anu_zac, col=TemperatureRamp(64), zlim=c(280,340), xlab=NA, ylab=NA, xaxt='n', yaxt='n')
	addCoastLines(Proj=PROJ_LATLON, Colour='black')
	legend('topleft', legend = paste0('(', letters[(t-1)*4+4], ')'), cex=4, bty='n')

}

dev.off()

# legend
par(cex=1.8)
image.plot(ZAC_log, col=TemperatureRamp(64), zlim=c(0,6), xlab = NA, ylab = NA, xaxt='n', yaxt='n')
par(cex=1.8)
image.plot(ZAC_log, col=vegeCol, zlim=c(2,10), xlab = NA, ylab = NA, xaxt='n', yaxt='n')

########################
# zoomed examples of ZAC
########################
source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_SetupForGraphics.R')
library(RColorBrewer); library(viridis)
library(rgdal)
library(pals)
library(fields)

# ibra boundary
ibra_shp    = shapefile('/datasets/work/d61-af-soilmoisture/work/AUS_boundaries/IBRA7_subregions_states/IBRA7_subregions_states.shp')
ibra_latlon = spTransform(ibra_shp, CRS('+proj=longlat +datum=WGS84'))
ext = floor(extent(ibra_latlon))
rr = raster(ext, res=0.1)
rr = rasterize(ibra_latlon, rr, field=1)
plot(ibra_shp, col='lightgrey', cex.axis=2)

# different surface reflectance data
ZAC_rst = raster('/datasets/work/d61-af-soilmoisture/work/himawari/ZAC_v1.4.1/Brent/ZAC_v1.4.1_log_par1_2km_1to99percentile.tif')
lai_rst = raster('/datasets/work/d61-af-soilmoisture/work/himawari/pixelwise_metrics/additional_indices/MCD15A2H_lai_median.tif')

evi_rst = raster('/datasets/work/d61-af-soilmoisture/work/himawari/pixelwise_metrics/additional_indices/MOD13A1_evi_median.tif')
evi_rst[evi_rst < 0.05] = 0.05; evi_rst[evi_rst > 0.6] = 0.6

alb_rst = raster('/datasets/work/d61-af-soilmoisture/work/himawari/pixelwise_metrics/additional_indices/MCD43A3_albedo_median.tif')
alb_rst[alb_rst < 0.05] = NA; alb_rst[alb_rst > 0.4] = 0.4

evi_rst = projectRaster(evi_rst, ZAC_rst, method='ngb')
lai_rst = projectRaster(lai_rst, ZAC_rst, method='ngb')
alb_rst = projectRaster(alb_rst, ZAC_rst, method='ngb')

aoi_set = c(extent(142,147,-20,-10), extent(123,134,-33,-28), extent(135,141,-32,-24), extent(148,154,-35,-25))

png('/datasets/work/d61-af-soilmoisture/work/himawari/figures/ZAC_spatial_pattern.png', width=1800, height=2000)

layout(rbind(c(1,2,3,4), c(5,6,7,8), c(9,10,11,12), c(13,14,15,16), c(17,18,19,20)))
par(mar = c(0.2, 0, 0.2, 0))

# plot the figures
plot(ZAC_rst, col=vegeRamp(64), zlim=c(2,10), legend = FALSE, xlab = NA, ylab = NA, xaxt='n', yaxt='n')
addCoastLines(Proj=PROJ_LATLON, Colour='black')
legend('topleft', legend = '(a)', cex=4, bty='n')
rect(142,-20,147,-10, lwd=2.5); rect(123,-33,134,-28, lwd=2.5); rect(135,-32,141,-24, lwd=2.5); rect(148,-35,154,-25, lwd=2.5)

plot(evi_rst, col=coolwarm(64), zlim=c(0,0.6), legend = FALSE, xlab = NA, ylab = NA, xaxt='n', yaxt='n')
addCoastLines(Proj=PROJ_LATLON, Colour='black')
legend('topleft', legend = '(b)', cex=4, bty='n')
rect(142,-20,147,-10, lwd=2.5); rect(123,-33,134,-28, lwd=2.5); rect(135,-32,141,-24, lwd=2.5); rect(148,-35,154,-25, lwd=2.5)

plot(lai_rst, col=coolwarm(64), zlim=c(0,7), legend = FALSE, xlab = NA, ylab = NA, xaxt='n', yaxt='n')
addCoastLines(Proj=PROJ_LATLON, Colour='black')
legend('topleft', legend = '(c)', cex=4, bty='n')
rect(142,-20,147,-10, lwd=2.5); rect(123,-33,134,-28, lwd=2.5); rect(135,-32,141,-24, lwd=2.5); rect(148,-35,154,-25, lwd=2.5)

plot(alb_rst, col=coolwarm(64), zlim=c(0,0.4), legend = FALSE, xlab = NA, ylab = NA, xaxt='n', yaxt='n')
addCoastLines(Proj=PROJ_LATLON, Colour='black')
legend('topleft', legend = '(d)', cex=4, bty='n')
rect(142,-20,147,-10, lwd=2.5); rect(123,-33,134,-28, lwd=2.5); rect(135,-32,141,-24, lwd=2.5); rect(148,-35,154,-25, lwd=2.5)

for (t in 1:4){

	ZAC_zoomed = crop(ZAC_rst, aoi_set[[t]]); plot(ZAC_zoomed, col=vegeRamp(64), zlim=c(2,10), legend = FALSE, xlab = NA, ylab = NA, xaxt='n', yaxt='n')
	ZAC_vec = as.vector(as.matrix(ZAC_zoomed))
	addCoastLines(Proj=PROJ_LATLON, Colour='black')
	legend('topleft', legend = paste0('(', letters[t*4+1], ')'), cex=4, bty='n')

	evi_zoomed = crop(evi_rst, aoi_set[[t]]); plot(evi_zoomed, col=coolwarm(64), zlim=c(0,0.6), legend = FALSE, xlab = NA, ylab = NA, xaxt='n', yaxt='n')
	evi_vec = as.vector(as.matrix(evi_zoomed))
	addCoastLines(Proj=PROJ_LATLON, Colour='black')
	legend('topleft', legend = paste0('(', letters[t*4+2], ')'), cex=4, bty='n')
	#legend('bottomright', legend = paste0('R = ', round(cor(ZAC_vec, evi_vec, use='complete.obs'), 2)), cex=4, bty='n')

	lai_zoomed = crop(lai_rst, aoi_set[[t]]); plot(lai_zoomed, col=coolwarm(64), zlim=c(0,7), legend = FALSE, xlab = NA, ylab = NA, xaxt='n', yaxt='n')
	lai_vec = as.vector(as.matrix(lai_zoomed))
	addCoastLines(Proj=PROJ_LATLON, Colour='black')
	legend('topleft', legend = paste0('(', letters[t*4+3], ')'), cex=4, bty='n')
	#legend('bottomright', legend = paste0('R = ', round(cor(ZAC_vec, lai_vec, use='complete.obs'), 2)), cex=4, bty='n')

	alb_zoomed = crop(alb_rst, aoi_set[[t]]); plot(alb_zoomed, col=coolwarm(64), zlim=c(0,0.4), legend = FALSE, xlab = NA, ylab = NA, xaxt='n', yaxt='n')
	alb_vec = as.vector(as.matrix(alb_zoomed))
	addCoastLines(Proj=PROJ_LATLON, Colour='black')
	legend('topleft', legend = paste0('(', letters[t*4+4], ')'), cex=4, bty='n')
	#legend('bottomright', legend = paste0('R = ', round(cor(ZAC_vec, alb_vec, use='complete.obs'), 2)), cex=4, bty='n')
}

dev.off()

###############################
# boxplots of land cover groups
###############################

source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_SetupForGraphics.R')
library(ggsci); library(pals); library(vioplot)

SCICOL = pal_nejm('default', alpha=0.7)(5)

# different surface reflectance data
ZAC_rst = raster('/datasets/work/d61-af-soilmoisture/work/himawari/ZAC_v1.4.1/Brent/ZAC_v1.4.1_log_par1_2km_1to99percentile.tif')
lai_rst = raster('/datasets/work/d61-af-soilmoisture/work/himawari/pixelwise_metrics/additional_indices/MCD15A2H_lai_median.tif')

evi_rst = raster('/datasets/work/d61-af-soilmoisture/work/himawari/pixelwise_metrics/additional_indices/MOD13A1_evi_median.tif')
evi_rst[evi_rst < 0.05] = 0.05; evi_rst[evi_rst > 0.6] = 0.6

alb_rst = raster('/datasets/work/d61-af-soilmoisture/work/himawari/pixelwise_metrics/additional_indices/MCD43A3_albedo_median.tif')
alb_rst[alb_rst < 0.05] = NA; alb_rst[alb_rst > 0.4] = 0.4

evi_rst = projectRaster(evi_rst, ZAC_rst, method='ngb')
lai_rst = projectRaster(lai_rst, ZAC_rst, method='ngb')
alb_rst = projectRaster(alb_rst, ZAC_rst, method='ngb')

# convert to vector
ZAC_vec = as.vector(as.matrix(ZAC_rst))
evi_vec = as.vector(as.matrix(evi_rst))
lai_vec = as.vector(as.matrix(lai_rst))
alb_vec = as.vector(as.matrix(alb_rst))

# data frame
aus_df = data.frame(zac=ZAC_vec, evi=evi_vec, lai=lai_vec, alb=alb_vec)

rowid_evi_0.15 = which(aus_df$evi <= 0.15)
rowid_evi_0.30 = which(aus_df$evi <= 0.30 & aus_df$evi > 0.15)
rowid_evi_0.45 = which(aus_df$evi <= 0.45 & aus_df$evi > 0.30)
rowid_evi_0.60 = which(aus_df$evi <= 0.60 & aus_df$evi > 0.45)

zac_val_evi_group1 = aus_df$zac[rowid_evi_0.15]
zac_val_evi_group2 = aus_df$zac[rowid_evi_0.30]
zac_val_evi_group3 = aus_df$zac[rowid_evi_0.45]
zac_val_evi_group4 = aus_df$zac[rowid_evi_0.60]

rowid_lai_1.75 = which(aus_df$lai <= 1.75)
rowid_lai_3.50 = which(aus_df$lai <= 3.50 & aus_df$lai > 1.75)
rowid_lai_5.25 = which(aus_df$lai <= 5.25 & aus_df$lai > 3.50)
rowid_lai_7.00 = which(aus_df$lai <= 7.00 & aus_df$lai > 5.25)

zac_val_lai_group1 = aus_df$zac[rowid_lai_1.75]
zac_val_lai_group2 = aus_df$zac[rowid_lai_3.50]
zac_val_lai_group3 = aus_df$zac[rowid_lai_5.25]
zac_val_lai_group4 = aus_df$zac[rowid_lai_7.00]

rowid_alb_0.10 = which(aus_df$alb <= 0.10)
rowid_alb_0.20 = which(aus_df$alb <= 0.20 & aus_df$alb > 0.10)
rowid_alb_0.30 = which(aus_df$alb <= 0.30 & aus_df$alb > 0.20)
rowid_alb_0.40 = which(aus_df$alb <= 0.40 & aus_df$alb > 0.30)

zac_val_alb_group1 = aus_df$zac[rowid_alb_0.10]
zac_val_alb_group2 = aus_df$zac[rowid_alb_0.20]
zac_val_alb_group3 = aus_df$zac[rowid_alb_0.30]
zac_val_alb_group4 = aus_df$zac[rowid_alb_0.40]

png('/datasets/work/d61-af-soilmoisture/work/himawari/figures/ZAC_coeff_group_boxplots.png', width=2000, height=500)

layout(cbind(1,2,3))
par(mar = c(2,4,2,2))

# boxplot without outliers

boxplot(zac_val_evi_group1, zac_val_evi_group2, zac_val_evi_group3, zac_val_evi_group4, outline=FALSE, col=coolwarm(16)[c(2,6,10,14)], cex.axis=4, ylim=c(2,10))
legend('topleft', legend = paste0('(a) Sample number = ', formatC(nrow(na.omit(aus_df[,c(1,2)])), big.mark=',')), cex=4, bty='n')

zac_val_evi_median = c(median(zac_val_evi_group1, na.rm=TRUE), median(zac_val_evi_group2, na.rm=TRUE), 
						median(zac_val_evi_group3, na.rm=TRUE), median(zac_val_evi_group4, na.rm=TRUE))
points(1:4, zac_val_evi_median, col = 'red', pch = 19, cex = 4)
text(1:4, 2, labels = format(round(zac_val_evi_median, 2), nsmall = 2), col='red', cex = 4)

boxplot(zac_val_lai_group1, zac_val_lai_group2, zac_val_lai_group3, zac_val_lai_group4, outline=FALSE, col=coolwarm(16)[c(2,6,10,14)], cex.axis=4, ylim=c(2,10))
legend('topleft', legend = paste0('(b) Sample number = ', formatC(nrow(na.omit(aus_df[,c(1,3)])), big.mark=',')), cex=4, bty='n')

zac_val_lai_median = c(median(zac_val_lai_group1, na.rm=TRUE), median(zac_val_lai_group2, na.rm=TRUE), 
						median(zac_val_lai_group3, na.rm=TRUE), median(zac_val_lai_group4, na.rm=TRUE))
points(1:4, zac_val_lai_median, col = 'red', pch = 19, cex = 4)
text(1:4, 2, labels = format(round(zac_val_lai_median, 2), nsmall = 2), col='red', cex = 4)

boxplot(zac_val_alb_group1, zac_val_alb_group2, zac_val_alb_group3, zac_val_alb_group4, outline=FALSE, col=coolwarm(16)[c(2,6,10,14)], cex.axis=4, ylim=c(2,10))
legend('topleft', legend = paste0('(c) Sample number = ', formatC(nrow(na.omit(aus_df[,c(1,4)])), big.mark=',')), cex=4, bty='n')

zac_val_alb_median = c(median(zac_val_alb_group1, na.rm=TRUE), median(zac_val_alb_group2, na.rm=TRUE), 
						median(zac_val_alb_group3, na.rm=TRUE), median(zac_val_alb_group4, na.rm=TRUE))
points(1:4, zac_val_alb_median, col = 'red', pch = 19, cex = 4)
text(1:4, 2, labels = format(round(zac_val_alb_median, 2), nsmall = 2), col='red', cex = 4)

dev.off()

png('/datasets/work/d61-af-soilmoisture/work/himawari/figures/ZAC_coeff_group_vioplots.png', width=2000, height=600)

layout(cbind(1,2,3))
par(mar = c(2,4,2,2))

# vioplot without outliers

# evi
vioplot(zac_val_evi_group1, zac_val_evi_group2, zac_val_evi_group3, zac_val_evi_group4, outer=FALSE, col=coolwarm(16)[c(2,6,10,14)], xaxt='n', cex.axis=4, ylim=c(1,11))
legend('topleft', legend = paste0('(a) Sample number = ', formatC(nrow(na.omit(aus_df[,c(1,2)])), big.mark=',')), cex=4, bty='n')

zac_val_evi_median = c(median(zac_val_evi_group1, na.rm=TRUE), median(zac_val_evi_group2, na.rm=TRUE), 
						median(zac_val_evi_group3, na.rm=TRUE), median(zac_val_evi_group4, na.rm=TRUE))
zac_evi_number_perc = c(paste0('(', round(length(zac_val_evi_group1)/nrow(na.omit(aus_df[,c(1,2)])) * 100, 2), '%)'),
						paste0('(', round(length(zac_val_evi_group2)/nrow(na.omit(aus_df[,c(1,2)])) * 100, 2), '%)'),
						paste0('(', round(length(zac_val_evi_group3)/nrow(na.omit(aus_df[,c(1,2)])) * 100, 2), '%)'),
						paste0('(', round(length(zac_val_evi_group4)/nrow(na.omit(aus_df[,c(1,2)])) * 100, 2), '%)'))

points(1:4, zac_val_evi_median, col = 'red', pch = 19, cex = 4)
text(1:4, 2, labels = format(round(zac_val_evi_median, 2), nsmall = 2), col='red', cex = 4)
text(1:4, 1, labels = zac_evi_number_perc, col='red', cex = 4)

# lai
vioplot(zac_val_lai_group1, zac_val_lai_group2, zac_val_lai_group3, zac_val_lai_group4, outer=FALSE, col=coolwarm(16)[c(2,6,10,14)], xaxt='n', cex.axis=4, ylim=c(1,11))
legend('topleft', legend = paste0('(b) Sample number = ', formatC(nrow(na.omit(aus_df[,c(1,3)])), big.mark=',')), cex=4, bty='n')

zac_val_lai_median = c(median(zac_val_lai_group1, na.rm=TRUE), median(zac_val_lai_group2, na.rm=TRUE), 
						median(zac_val_lai_group3, na.rm=TRUE), median(zac_val_lai_group4, na.rm=TRUE))
zac_lai_number_perc = c(paste0('(', round(length(zac_val_lai_group1)/nrow(na.omit(aus_df[,c(1,3)])) * 100, 2), '%)'),
						paste0('(', round(length(zac_val_lai_group2)/nrow(na.omit(aus_df[,c(1,3)])) * 100, 2), '%)'),
						paste0('(', round(length(zac_val_lai_group3)/nrow(na.omit(aus_df[,c(1,3)])) * 100, 2), '%)'),
						paste0('(', round(length(zac_val_lai_group4)/nrow(na.omit(aus_df[,c(1,3)])) * 100, 2), '%)'))

points(1:4, zac_val_lai_median, col = 'red', pch = 19, cex = 4)
text(1:4, 2, labels = format(round(zac_val_lai_median, 2), nsmall = 2), col='red', cex = 4)
text(1:4, 1, labels = zac_lai_number_perc, col='red', cex = 4)

# alb
vioplot(zac_val_alb_group1, zac_val_alb_group2, zac_val_alb_group3, zac_val_alb_group4, outer=FALSE, col=coolwarm(16)[c(2,6,10,14)], xaxt='n', cex.axis=4, ylim=c(1,11))
legend('topleft', legend = paste0('(c) Sample number = ', formatC(nrow(na.omit(aus_df[,c(1,4)])), big.mark=',')), cex=4, bty='n')

zac_val_alb_median = c(median(zac_val_alb_group1, na.rm=TRUE), median(zac_val_alb_group2, na.rm=TRUE), 
						median(zac_val_alb_group3, na.rm=TRUE), median(zac_val_alb_group4, na.rm=TRUE))
zac_alb_number_perc = c(paste0('(', round(length(zac_val_alb_group1)/nrow(na.omit(aus_df[,c(1,4)])) * 100, 2), '%)'),
						paste0('(', round(length(zac_val_alb_group2)/nrow(na.omit(aus_df[,c(1,4)])) * 100, 2), '%)'),
						paste0('(', round(length(zac_val_alb_group3)/nrow(na.omit(aus_df[,c(1,4)])) * 100, 2), '%)'),
						paste0('(', round(length(zac_val_alb_group4)/nrow(na.omit(aus_df[,c(1,4)])) * 100, 2), '%)'))

points(1:4, zac_val_alb_median, col = 'red', pch = 19, cex = 4)
text(1:4, 2, labels = format(round(zac_val_alb_median, 2), nsmall = 2), col='red', cex = 4)
text(1:4, 1, labels = zac_alb_number_perc, col='red', cex = 4)

dev.off()
