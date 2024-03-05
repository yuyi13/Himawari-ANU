# ################################################################
# objective:   get median VIs and albedo to compare with SZAC
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_SetupForGraphics.R')
library(terra)
library(foreach)
library(doParallel)

cl <- makeCluster(48)
print(cl)
registerDoParallel(cl)

alb_path = '/datasets/work/d61-af-soilmoisture/work/MODIS_data/MCD43A3_Albedo/tiff'
lai_path = '/datasets/work/d61-af-soilmoisture/work/MODIS_data/MCD15A2H_LAI/tiff'
evi_path = '/datasets/work/d61-af-soilmoisture/work/MODIS_data/MOD13A1_VI/tiff'

alb_fl = list.files(alb_path, pattern='Albedo_WSA_shortwave_2016|Albedo_WSA_shortwave_2017|Albedo_WSA_shortwave_2018|Albedo_WSA_shortwave_2019|Albedo_WSA_shortwave_2020', full.names=TRUE)
lai_fl = list.files(lai_path, pattern='Lai_500m_2016|Lai_500m_2017|Lai_500m_2018|Lai_500m_2019|Lai_500m_2020', full.names=TRUE)
evi_fl = list.files(evi_path, pattern='EVI_500m_2016|EVI_500m_2017|EVI_500m_2018|EVI_500m_2019|EVI_500m_2020', full.names=TRUE)

print('--------------------------------------------')
print('| Start to prepare stacks for MODIS pixels |')
print('--------------------------------------------')

alb_stk = rast(alb_fl)

alb_mean   = app(alb_stk, fun=mean, na.rm=TRUE, cores=cl);   writeRaster(alb_mean, '/datasets/work/d61-af-soilmoisture/work/himawari/pixelwise_metrics/additional_indices/MCD43A3_albedo_mean.tif', overwrite=TRUE)
alb_median = app(alb_stk, fun=median, na.rm=TRUE, cores=cl); writeRaster(alb_median, '/datasets/work/d61-af-soilmoisture/work/himawari/pixelwise_metrics/additional_indices/MCD43A3_albedo_median.tif', overwrite=TRUE)
alb_max    = app(alb_stk, fun=max, na.rm=TRUE, cores=cl);    writeRaster(alb_max, '/datasets/work/d61-af-soilmoisture/work/himawari/pixelwise_metrics/additional_indices/MCD43A3_albedo_max.tif', overwrite=TRUE)
alb_min    = app(alb_stk, fun=min, na.rm=TRUE, cores=cl);    writeRaster(alb_min, '/datasets/work/d61-af-soilmoisture/work/himawari/pixelwise_metrics/additional_indices/MCD43A3_albedo_min.tif', overwrite=TRUE)

lai_stk = rast(lai_fl)

lai_mean   = app(lai_stk, fun=mean, na.rm=TRUE, cores=cl);   writeRaster(lai_mean, '/datasets/work/d61-af-soilmoisture/work/himawari/pixelwise_metrics/additional_indices/MCD15A2H_lai_mean.tif', overwrite=TRUE)
lai_median = app(lai_stk, fun=median, na.rm=TRUE, cores=cl); writeRaster(lai_median, '/datasets/work/d61-af-soilmoisture/work/himawari/pixelwise_metrics/additional_indices/MCD15A2H_lai_median.tif', overwrite=TRUE)
lai_max    = app(lai_stk, fun=max, na.rm=TRUE, cores=cl);    writeRaster(lai_max, '/datasets/work/d61-af-soilmoisture/work/himawari/pixelwise_metrics/additional_indices/MCD15A2H_lai_max.tif', overwrite=TRUE)
lai_min    = app(lai_stk, fun=min, na.rm=TRUE, cores=cl);    writeRaster(lai_min, '/datasets/work/d61-af-soilmoisture/work/himawari/pixelwise_metrics/additional_indices/MCD15A2H_lai_min.tif', overwrite=TRUE)

evi_stk = rast(evi_fl)

evi_mean   = app(evi_stk, fun=mean, na.rm=TRUE, cores=cl);   writeRaster(evi_mean, '/datasets/work/d61-af-soilmoisture/work/himawari/pixelwise_metrics/additional_indices/MOD13A1_evi_mean.tif', overwrite=TRUE)
evi_median = app(evi_stk, fun=median, na.rm=TRUE, cores=cl); writeRaster(evi_median, '/datasets/work/d61-af-soilmoisture/work/himawari/pixelwise_metrics/additional_indices/MOD13A1_evi_median.tif', overwrite=TRUE)
evi_max    = app(evi_stk, fun=max, na.rm=TRUE, cores=cl);    writeRaster(evi_max, '/datasets/work/d61-af-soilmoisture/work/himawari/pixelwise_metrics/additional_indices/MOD13A1_evi_max.tif', overwrite=TRUE)
evi_min    = app(evi_stk, fun=min, na.rm=TRUE, cores=cl);    writeRaster(evi_min, '/datasets/work/d61-af-soilmoisture/work/himawari/pixelwise_metrics/additional_indices/MOD13A1_evi_min.tif', overwrite=TRUE)

print('Finished metrics for MODIS during 2016 - 2020')
print(' ')
