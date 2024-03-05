# ################################################################
# objective:   retain hourly viirs lst data using utc hours (3-7)
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_SetupForGraphics.R')
library(foreach)
library(doParallel)

cl <- makeCluster(24)
print(cl)
registerDoParallel(cl)

Dates = seq(as.Date('2016-01-01'),as.Date('2020-12-31'),by='day')

# paths to files
path2vii = '/g/data/dt1/SatelliteLST/VIIRS_LST/'
path2cop = '/g/data/dt1/SatelliteLST/Copernicus_LST_AUS/'
path2chiba = '/g/data/dt1/SatelliteLST/H8_LST_Chiba/'
outpath = '/g/data/dt1/SatelliteLST/hourly_comparison/'

# Himawari-8 Oz template
tem = raster(extent(112,154,-45,-10), res=0.02, crs=PROJ_LATLON)

# Australian continent's longitude raster
lon_mtx = matrix(NA, nrow=1750, ncol=2100)
for (ll in 1:ncol(lon_mtx)) lon_mtx[,ll] = 112.01 + 0.02 * (ll-1)
lon_rst = raster(lon_mtx, xmn=112, xmx=154, ymn=-45, ymx=-10, crs=PROJ_LATLON)

time_range = 3:7

    foreach (k = 1:length(Dates), .packages=c('raster')) %dopar% {

        # paths to modis lst and time
	    path2lst  = paste0(path2vii, format(Dates[k], '/LST/%Y/'))
        path2time = paste0(path2vii, format(Dates[k], '/Time/%Y/'))

        # find the files
        lst  = paste0(path2lst,  format(Dates[k], '%Y%m%d_'), 'VNP21A1D_LST_AusSubset.tif')
        time = paste0(path2time, format(Dates[k], '%Y%m%d_'), 'VNP21A1D_Time_AusSubset.tif')

        if (file.exists(lst)){

            # read data as raster
            rst_lst = raster(lst)
            rst_time = raster(time); rst_time = mask(rst_time, rst_lst)

            rst_lst = projectRaster(rst_lst, tem, method='ngb')
            rst_time = projectRaster(rst_time, tem, method='ngb')
        
            # convert viirs time to utc
            viitime = rst_time - lon_rst/15

        } else {

            rst_lst = tem
            viitime = tem

        }
        # outpath 
        outpath_vii = paste0(outpath, 'VIIRS/', format(Dates[k], '%Y/%m/%d/'))
        if (!dir.exists(outpath_vii)) dir.create(outpath_vii, recursive=TRUE)

        outpath_chiba = paste0(outpath, 'Chiba/', format(Dates[k], '%Y/%m/%d/'))
        if (!dir.exists(outpath_chiba)) dir.create(outpath_chiba, recursive=TRUE)

        outpath_cop = paste0(outpath, 'Copernicus/', format(Dates[k], '%Y/%m/%d/'))
        if (!dir.exists(outpath_cop)) dir.create(outpath_cop, recursive=TRUE)

        for (t in time_range){
            
            # himawari-8 chiba lst
            chiba_lst = paste0(path2chiba, format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '00_AHI_Chiba_LSTv0_AusSubset.tif')
            cop_lst = paste0(path2cop, format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '00_C_GLS_GEO_LSTv1.2.1_AusSubset.tiff')

            if (file.exists(chiba_lst) & file.exists(cop_lst)){
                
                chiba_lst = raster(chiba_lst)
                cop_lst = raster(cop_lst) * 0.01 + 273.15
                cop_lst = projectRaster(cop_lst, tem, method='ngb')
   
                # the viirs utc time; will be used as mask to extract himawari-8 and viirs lst data
                selected_time = viitime
                selected_time[selected_time < t-0.5 | selected_time > t+0.5] = NA

                # extract himawari-8 and viirs lst data
                chiba_lst = mask(chiba_lst, selected_time)
                cop_lst = mask(cop_lst, selected_time)
                vii_lst = mask(rst_lst, selected_time)

            } else {

                chiba_lst = tem
                cop_lst = tem
                vii_lst = tem
            }

            # save the data
            writeRaster(chiba_lst, paste0(outpath, 'Chiba/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '00_AHI_Chiba_LSTv0_AusSubset.tif'), overwrite=TRUE)
            writeRaster(cop_lst, paste0(outpath, 'Copernicus/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '00_C_GLS_GEO_LSTv1.2.1_AusSubset.tiff'), overwrite=TRUE)
            writeRaster(vii_lst, paste0(outpath, 'VIIRS/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '00_VNP21A1D_LST_AusSubset.tif'), overwrite=TRUE)

        }
    }
