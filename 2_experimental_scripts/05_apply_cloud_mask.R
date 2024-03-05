# ################################################################
# objective:   apply himawari chiba cloud mask to anu lst
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

path2baseline = '/datasets/work/d61-af-soilmoisture/work/SatelliteLST/ANU_baseline/'
path2chiba    = '/datasets/work/d61-af-soilmoisture/work/SatelliteLST/Chiba/AusSubset/'
path2calib    = '/datasets/work/d61-af-soilmoisture/work/SatelliteLST/ANU_calib/'

outpath_base  = '/datasets/work/d61-af-soilmoisture/work/SatelliteLST/ANU_baseline_masked/'
outpath_calib = '/datasets/work/d61-af-soilmoisture/work/SatelliteLST/ANU_calib_masked/'

TOIs = seq(as.POSIXct('2016-01-01 00:00:00'), as.POSIXct('2020-12-31 23:00:00'), by='hour')

foreach (k = 1:length(TOIs), .combine=cbind, .packages=c('terra', 'raster')) %dopar% {

    baseline_lst = raster(paste0(path2baseline, format(TOIs[k], '%Y/%m/%d/%Y%m%d%H0000_AHI_ANU_LSTv1.0_AusSubset.tif')))
    chiba_lst    = raster(paste0(path2chiba, format(TOIs[k], '%Y/%m/%d/%Y%m%d%H00_AHI_Chiba_LSTv0_AusSubset.tif')))
    calib_lst    = raster(paste0(path2calib, format(TOIs[k], '%Y/%m/%d/%Y%m%d%H0000_AHI_ANU_LSTv1.4.1_AusSubset.tif')))

    # cloud mask
    baseline_lst = mask(baseline_lst, chiba_lst)
    calib_lst    = mask(calib_lst, chiba_lst)

    # out dir
    outdir_base  = paste0(outpath_base, format(TOIs[k], '%Y/%m/%d/'))
    if (!dir.exists(outdir_base)) dir.create(outdir_base, recursive=TRUE)

    outdir_calib = paste0(outpath_calib, format(TOIs[k], '%Y/%m/%d/'))
    if (!dir.exists(outdir_calib)) dir.create(outdir_calib, recursive=TRUE)

    # write to file
    writeRaster(baseline_lst, paste0(outdir_base, format(TOIs[k], '%Y%m%d%H0000_AHI_ANU_LSTv1.0_AusSubset.tif')), overwrite=TRUE)
    writeRaster(calib_lst,    paste0(outdir_calib, format(TOIs[k], '%Y%m%d%H0000_AHI_ANU_LSTv1.4.1_AusSubset.tif')), overwrite=TRUE)
}
