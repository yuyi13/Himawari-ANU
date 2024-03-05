# ################################################################
# objective:   retain hourly modis lst data using utc hours (0-8)
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

name_list = c('MOD11A1', 'MYD11A1')
Dates = seq(as.Date('2016-01-01'),as.Date('2022-12-31'),by='day')

# paths to files
path2anu = '/g/data/lr26/Himawari-AHI_LST_ANU/' # ANU_v1.4.1
path2modis = '/g/data/dt1/SatelliteLST/MODIS_LST/'
outpath = '/g/data/dt1/SatelliteLST/hourly_comparison/'

# Himawari-8 Oz template
tem = raster(extent(112,154,-45,-10), res=0.02, crs=PROJ_LATLON)

# Australian continent's longitude raster
lon_mtx = matrix(NA, nrow=1750, ncol=2100)
for (ll in 1:ncol(lon_mtx)) lon_mtx[,ll] = 112.01 + 0.02 * (ll-1)
lon_rst = raster(lon_mtx, xmn=112, xmx=154, ymn=-45, ymx=-10, crs=PROJ_LATLON)

for (m in 1:2){

    # get the UTC time range for Terra/Aqua
    if (m == 1) {
        time_range = 0:4
    } else {
        time_range = 4:8
    }

    print(paste0('start to do the ', name_list[m], ' data'))

    foreach (k = 1:length(Dates), .packages=c('raster')) %dopar% {

        # paths to modis lst and time
	    path2lst  = paste0(path2modis, name_list[m], format(Dates[k], '/LST/%Y/'))
        path2time = paste0(path2modis, name_list[m], format(Dates[k], '/Time/%Y/'))

        # find the files
        lst  = paste0(path2lst,  format(Dates[k], '%Y%m%d_'), name_list[m], '_LST_AusSubset.tif')
        time = paste0(path2time, format(Dates[k], '%Y%m%d_'), name_list[m], '_Time_AusSubset.tif')

        if (file.exists(lst)){

            # read data as raster
            rst_lst = raster(lst)
            rst_time = raster(time); rst_time = mask(rst_time, rst_lst)

            rst_lst = projectRaster(rst_lst, tem, method='ngb')
            rst_time = projectRaster(rst_time, tem, method='ngb')
        
            # convert modis time to utc
            modtime = rst_time - lon_rst/15

        } else {

            rst_lst = tem
            modtime = tem

        }
        # outpath 
        outpath_anu = paste0(outpath, 'ANUcalib/', format(Dates[k], '%Y/%m/%d/'))
        if (!dir.exists(outpath_anu)) dir.create(outpath_anu, recursive=TRUE)

        outpath_mod = paste0(outpath, name_list[m], '/', format(Dates[k], '%Y/%m/%d/'))
        if (!dir.exists(outpath_mod)) dir.create(outpath_mod, recursive=TRUE)

        for (t in time_range){
            
            # himawari-8 anu lst v1.4.1
            anu_lst = paste0(path2anu, format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '0000_AHI_ANU_LSTv1.4.1_AusSubset.tif')

            if (file.exists(anu_lst)){
                
                anu_lst = raster(anu_lst)
   
                # the modis utc time; will be used as mask to extract himawari-8 and modis lst data
                selected_time = modtime
                selected_time[selected_time < t-0.5 | selected_time > t+0.5] = NA

                # extract himawari-8 and modis lst data
                anu_lst = mask(anu_lst, selected_time)
                mod_lst = mask(rst_lst, selected_time)

            } else {

                anu_lst = tem
                mod_lst = tem
            }

            # save the data
            writeRaster(anu_lst, paste0(outpath, 'ANUcalib/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '00_AHI_ANU_LSTv1.4.1_AusSubset.tif'), overwrite=TRUE)
            writeRaster(mod_lst, paste0(outpath, name_list[m], '/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '00_', 
                        name_list[m], '_LST_AusSubset.tif'), overwrite=TRUE)

        }
    }
}
