# ################################################################
# objective:   calculate pixelwise diff for modis best pixels
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

path2satelliteLST = '/g/data/dt1/SatelliteLST/'
processed_path = '/scratch/os22/yy4778/MODIS_data/processed/'
path2anucalib = '/g/data/lr26/Himawari-AHI_LST_ANU/'
out_path = '/g/data/dt1/SatelliteLST/hourly_comparison/Diff/'

if (!dir.exists(paste0(out_path, 'ANU-MODISbest'))) dir.create(paste0(out_path, 'ANU-MODISbest'))
if (!dir.exists(paste0(out_path, 'ANUcalib-MODISbest'))) dir.create(paste0(out_path, 'ANUcalib-MODISbest'))

aus_template = raster(extent(112, 154, -45, -10), res=0.02, crs=PROJ_LATLON)

## This part is to calculate MODIS fitting period (2016 - 2020) pixelwise bias and ubRMSE
Dates = seq(as.Date('2016-01-01'),as.Date('2020-12-31'),by='day')
time_range = 0:8

print('Start preparing data for 2016 - 2020')

foreach (k = 1:length(Dates), .packages=c('raster')) %dopar% {

    for (t in time_range){

        anu_lst = paste0(path2satelliteLST, 'H8_LST_ANU_baseline/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '0000_AHI_ANU_LSTv1.0_AusSubset.tif')
        anu_calib_lst = paste0(path2anucalib, format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '0000_AHI_ANU_LSTv1.4.1_AusSubset.tif')
        mod_lst = paste0(processed_path, 'MODIS/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '00_MODIS_LST_AusSubset.tif')

        if (file.exists(anu_lst) & file.exists(anu_calib_lst) & file.exists(mod_lst)){

        anu_lst = raster(anu_lst)
        anu_calib_lst = raster(anu_calib_lst)
        mod_lst = raster(mod_lst)

        anu_minus_mod = anu_lst - mod_lst
        anu_calib_minus_mod = anu_calib_lst - mod_lst

        anu_minus_mod[anu_minus_mod < -10 | anu_minus_mod > 10] = NA
        anu_calib_minus_mod[anu_calib_minus_mod < -10 | anu_calib_minus_mod > 10] = NA

        writeRaster(anu_minus_mod, paste0(out_path, 'ANU-MODISbest/', format(Dates[k], '%Y%m%d0'), t, '00_ANU_minus_MODISbest.tif'), overwrite=TRUE)
        writeRaster(anu_calib_minus_mod, paste0(out_path, 'ANUcalib-MODISbest/', format(Dates[k], '%Y%m%d0'), t, '00_ANUcalib_minus_MODISbest.tif'), overwrite=TRUE)
    }}
}
