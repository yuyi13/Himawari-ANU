# ################################################################
# objective:   prepare viirs rsts for directionality analysis
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_SetupForGraphics.R')
library(foreach)
library(doParallel)

cl <- makeCluster(48)
print(cl)
registerDoParallel(cl)

Dates = seq(as.Date('2016-01-01'),as.Date('2020-12-31'),by='day')

# paths to files
path2vii = '/datasets/work/d61-af-soilmoisture/work/SatelliteLST/VIIRS/'
outpath = '/datasets/work/d61-af-soilmoisture/work/himawari/hourly_comparison/'

# Himawari-8 Oz template
tem = raster(extent(112,154,-45,-10), res=0.02, crs=PROJ_LATLON)

# Australian continent's longitude raster
lon_mtx = matrix(NA, nrow=1750, ncol=2100)
for (ll in 1:ncol(lon_mtx)) lon_mtx[,ll] = 112.01 + 0.02 * (ll-1)
lon_rst = raster(lon_mtx, xmn=112, xmx=154, ymn=-45, ymx=-10, crs=PROJ_LATLON)

# this means GMT time (from 3:00 to 7:00)
time_range = 3:7

    foreach (k = 1:length(Dates), .packages=c('raster')) %dopar% {

        # paths to modis lst and time
	    path2lst  = paste0(path2vii, format(Dates[k], 'LST/%Y/'))
        path2time = paste0(path2vii, format(Dates[k], 'Time/%Y/'))
        path2vza  = paste0(path2vii, format(Dates[k], 'VZA/%Y/'))

        # find the files
        lst  = paste0(path2lst,  format(Dates[k], '%Y%m%d_'), 'VNP21A1D_LST_AusSubset.tif')
        time = paste0(path2time, format(Dates[k], '%Y%m%d_'), 'VNP21A1D_Time_AusSubset.tif')
        vza  = paste0(path2vza,  format(Dates[k], '%Y%m%d_'), 'VNP21A1D_VZA_AusSubset.tif')

        if (file.exists(lst)){

            # read data as raster
            rst_lst = raster(lst)
            rst_time = raster(time)
            rst_vza = raster(vza)

            rst_lst = projectRaster(rst_lst, tem, method='ngb')
            rst_time = projectRaster(rst_time, tem, method='ngb')
            rst_vza = projectRaster(rst_vza, tem, method='ngb')
        
            # convert viirs time to utc
            viitime = rst_time - lon_rst/15

        } else {

            rst_lst = tem
            viitime = tem
            rst_vza = tem

        }
        # outpath 
        outpath_lst = paste0(outpath, 'VIIRS/LST/', format(Dates[k], '%Y/%m/%d/'))
        if (!dir.exists(outpath_lst)) dir.create(outpath_lst, recursive=TRUE)

        outpath_vza = paste0(outpath, 'VIIRS/VZA/', format(Dates[k], '%Y/%m/%d/'))
        if (!dir.exists(outpath_vza)) dir.create(outpath_vza, recursive=TRUE)

        for (t in time_range){
            
            # the viirs utc time; will be used as mask to extract viirs lst and vza data
            selected_time = viitime
            selected_time[selected_time < t-0.5 | selected_time > t+0.5] = NA

            # extract himawari-8 and viirs lst data
            vii_lst = mask(rst_lst, selected_time)
            vii_vza = mask(rst_vza, selected_time)
            #vii_vza[vii_vza > 50 | vii_vza < (-50)] = NA

            # save the data
            writeRaster(vii_lst, paste0(outpath, 'VIIRS/LST/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '00_VNP21A1D_LST_AusSubset.tif'), overwrite=TRUE)
            writeRaster(vii_vza, paste0(outpath, 'VIIRS/VZA/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '00_VNP21A1D_VZA_AusSubset.tif'), overwrite=TRUE)
        }
    }
