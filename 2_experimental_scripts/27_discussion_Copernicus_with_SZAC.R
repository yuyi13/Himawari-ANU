# ################################################################
# objective:   apply szac to copernicus lst data
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_SetupForGraphics.R')
library(terra)
library(foreach)
library(doParallel)

cl <- makeCluster(24)
print(cl)
registerDoParallel(cl)

TOIs = seq(as.POSIXct('2016-01-01 00:00:00'), as.POSIXct('2020-12-31 23:00:00'), by='hour')

szac_coeff = raster('/datasets/work/d61-af-soilmoisture/work/himawari/Copernicus_ZAC/Brent/SZAC_Copernicus_log_par1_2km_1to99percentile.tif')

copernicus_path = '/datasets/work/d61-af-soilmoisture/work/SatelliteLST/Copernicus/AusSubset/'
sza_path        = '/datasets/work/d61-af-soilmoisture/work/SatelliteLST/Himawari_SZA/'
output_path     = '/datasets/work/d61-af-soilmoisture/work/himawari/processed/Copernicus_SZAC/'

if (!dir.exists(output_path)) dir.create(output_path, recursive=TRUE)

foreach(k = 1:length(TOIs), .combine=cbind, .packages=c('terra', 'raster')) %dopar% {

    cop_rst  = raster(paste0(copernicus_path, format(TOIs[k], '%Y/%m/%d/%Y%m%d%H'), '00_C_GLS_GEO_LSTv1.2.1_AusSubset.tiff'))
    cop_rst  = projectRaster(cop_rst, szac_coeff, method='ngb')

    sza_rst  = raster(paste0(sza_path, format(TOIs[k], '%Y/%m/%d/%Y%m%d%H'), '00_Himawari_SolarZenithAngle_AusSubset.tif'))
    sza_rst[sza_rst > 85] = NA # only retain the data with solar zenith angle less than 85 degrees (daytime)
    szac_rst = szac_coeff * log(cos(sza_rst * pi/180) + 1)

    cop_calib = cop_rst - szac_rst

    output = paste0(output_path, format(TOIs[k], '%Y/%m/%d/'))
    if (!dir.exists(output)) dir.create(output, recursive=TRUE)

    writeRaster(cop_calib, paste0(output_path, format(TOIs[k], '%Y/%m/%d/%Y%m%d%H'), '00_C_GLS_GEO_LSTv1.2.1_SZAC_AusSubset.tif'), overwrite=TRUE)
}
