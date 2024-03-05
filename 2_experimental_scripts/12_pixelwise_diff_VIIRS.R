# ################################################################
# objective:   calculate pixelwise diff for viirs all pixels
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
path2anucalib = '/g/data/lr26/Himawari-AHI_LST_ANU/'
path2viirs = '/g/data/dt1/SatelliteLST/hourly_comparison/VIIRS/'
out_path = '/g/data/dt1/SatelliteLST/hourly_comparison/Diff/'

# dirs for VIIRS
if (!dir.exists(paste0(out_path, 'ANU-VIIRS'))) dir.create(paste0(out_path, 'ANU-VIIRS'))
if (!dir.exists(paste0(out_path, 'ANUcalib-VIIRS'))) dir.create(paste0(out_path, 'ANUcalib-VIIRS'))
if (!dir.exists(paste0(out_path, 'Chiba-VIIRS'))) dir.create(paste0(out_path, 'Chiba-VIIRS'))
if (!dir.exists(paste0(out_path, 'Copernicus-VIIRS'))) dir.create(paste0(out_path, 'Copernicus-VIIRS'))

aus_template = raster(extent(112, 154, -45, -10), res=0.02, crs=PROJ_LATLON)

## This part is to calculate VIIRS fitting period (2016 - 2020) pixelwise bias and ubRMSE
Dates = seq(as.Date('2016-01-01'),as.Date('2020-12-31'),by='day')

print('Start preparing data for 2016 - 2020')

time_range = 3:7

foreach (k = 1:length(Dates), .packages=c('raster')) %dopar% {

    for (t in time_range){

        anu_lst   = paste0(path2satelliteLST, 'H8_LST_ANU_baseline/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '0000_AHI_ANU_LSTv1.0_AusSubset.tif')
        chiba_lst = paste0(path2satelliteLST, 'H8_LST_Chiba/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '00_AHI_Chiba_LSTv0_AusSubset.tif')
        cop_lst   = paste0(path2satelliteLST, 'Copernicus_LST_AUS/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '00_C_GLS_GEO_LSTv1.2.1_AusSubset.tiff')
        anu_calib_lst = paste0(path2anucalib, format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '0000_AHI_ANU_LSTv1.4.1_AusSubset.tif')
        vii_lst   = paste0(path2viirs, format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '00_VNP21A1D_LST_AusSubset.tif')

        if (file.exists(anu_lst) & file.exists(chiba_lst) & file.exists(cop_lst) & file.exists(anu_calib_lst) & file.exists(vii_lst)){
        
        # read the lst data
        # anu product does not have cloud mask so use chiba cloud mask
        # copernicus lst is ~ 4.5 km resolution, so resample to 2 km
        anu_lst   = raster(anu_lst)
        chiba_lst = raster(chiba_lst); anu_lst = mask(anu_lst, chiba_lst)
        cop_lst   = raster(cop_lst) * 0.01 + 273.15; cop_lst = projectRaster(cop_lst, aus_template, method='ngb')
        anu_calib_lst = raster(anu_calib_lst); anu_calib_lst = mask(anu_calib_lst, chiba_lst)
        vii_lst   = raster(vii_lst)

        anu_minus_vii   = anu_lst - vii_lst
        chiba_minus_vii = chiba_lst - vii_lst
        cop_minus_vii   = cop_lst - vii_lst
        anu_calib_minus_vii = anu_calib_lst - vii_lst

        anu_minus_vii[anu_minus_vii < -10 | anu_minus_vii > 10] = NA
        chiba_minus_vii[chiba_minus_vii < -10 | chiba_minus_vii > 10] = NA
        cop_minus_vii[cop_minus_vii < -10 | cop_minus_vii > 10] = NA
        anu_calib_minus_vii[anu_calib_minus_vii < -10 | anu_calib_minus_vii > 10] = NA

        writeRaster(anu_minus_vii, paste0(out_path, 'ANU-VIIRS/', format(Dates[k], '%Y%m%d0'), t, '00_ANU_minus_VIIRS.tif'), overwrite=TRUE)
        writeRaster(chiba_minus_vii, paste0(out_path, 'Chiba-VIIRS/', format(Dates[k], '%Y%m%d0'), t, '00_Chiba_minus_VIIRS.tif'), overwrite=TRUE)
        writeRaster(cop_minus_vii, paste0(out_path, 'Copernicus-VIIRS/', format(Dates[k], '%Y%m%d0'), t, '00_Copernicus_minus_VIIRS.tif'), overwrite=TRUE)
        writeRaster(anu_calib_minus_vii, paste0(out_path, 'ANUcalib-VIIRS/', format(Dates[k], '%Y%m%d0'), t, '00_ANUcalib_minus_VIIRS.tif'), overwrite=TRUE)
    }}
}

print('finish tasks.')
