# ################################################################
# objective:   prepare copernicus lst rsts at utc hours 
#              for additional experiments
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_SetupForGraphics.R')
library(terra)
library(foreach)
library(doParallel)

cl <- makeCluster(40)
print(cl)
registerDoParallel(cl)

Dates = seq(as.Date('2016-01-01'),as.Date('2020-12-31'),by='day')

processed_path = '/datasets/work/d61-af-soilmoisture/work/himawari/processed/'
copernicus_path = '/datasets/work/d61-af-soilmoisture/work/SatelliteLST/Copernicus/AusSubset/'

aus_template = raster(extent(112, 154, -45, -10), res=0.02, crs=PROJ_LATLON)

time_range = 0:8

foreach (k = 1:length(Dates), .combine=cbind, .packages=c('terra', 'raster')) %dopar% {

    outpath = paste0(processed_path, 'Copernicus/', format(Dates[k], '%Y/%m/%d/'))
    if (!dir.exists(outpath)) dir.create(outpath, recursive=TRUE)

    for (j in 1:length(time_range)){
        
        cop_lst = paste0(copernicus_path, format(Dates[k], '%Y/%m/%d/%Y%m%d0'), time_range[j], '00_C_GLS_GEO_LSTv1.2.1_AusSubset.tiff')
        mod_lst = paste0(processed_path, 'MODIS/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), time_range[j], '00_MODIS_LST_AusSubset.tif')
        
        if (file.exists(cop_lst)){
            
            cop_rst = raster(cop_lst); mod_rst = raster(mod_lst)
            cop_rst = projectRaster(cop_rst, aus_template, method='ngb')
            cop_rst = mask(cop_rst, mod_rst)

            writeRaster(cop_rst, paste0(processed_path, 'Copernicus/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), time_range[j], '00_C_GLS_GEO_LSTv1.2.1_AusSubset.tif'),
                        overwrite=TRUE)
        } else {

            print(paste0('Copernicus file does not exist: ', cop_lst))
            writeRaster(aus_template, paste0(processed_path, 'Copernicus/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), time_range[j], '00_C_GLS_GEO_LSTv1.2.1_AusSubset.tif'),
                        overwrite=TRUE)
        }
    }
}
