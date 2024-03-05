# ################################################################
# objective:   prepare raster stacks of Himawari-8 and MODIS using 
#              a ±30min temporal window
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
Dates = seq(as.Date('2016-01-01'),as.Date('2020-12-31'),by='day')

# paths to files
path2anu  = '/g/data/dt1/SatelliteLST/H8_LST_ANU_baseline/'
path2angle= '/g/data/ra22/satellite-products/arc/obs/himawari-ahi/fldk/latest/'
path2data = '/g/data/dt1/SatelliteLST/MODIS_LST/'
outpath   = '/scratch/os22/yy4778/MODIS_data/processed/'

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
	    path2lst_qc = paste0(path2data, name_list[m], format(Dates[k], '/QualityControled_LST/%Y/'))
        path2time   = paste0(path2data, name_list[m], format(Dates[k], '/Time/%Y/'))

        # find the files
        lst = paste0(path2lst_qc, format(Dates[k], '%Y%m%d_'), name_list[m], '_QualityControled_LST_AusSubset.tif')
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
        outpath_anu = paste0(outpath, 'ANU/', format(Dates[k], '%Y/%m/%d/'))
        if (!dir.exists(outpath_anu)) dir.create(outpath_anu, recursive=TRUE)

        outpath_mod = paste0(outpath, 'MODIS/', format(Dates[k], '%Y/%m/%d/'))
        if (!dir.exists(outpath_mod)) dir.create(outpath_mod, recursive=TRUE)

        outpath_sza = paste0(outpath, 'SZA/', format(Dates[k], '%Y/%m/%d/'))
        if (!dir.exists(outpath_sza)) dir.create(outpath_sza, recursive=TRUE)

        for (t in time_range){
            
            # himawari-8 anu lst
            anu_lst = paste0(path2anu, format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '0000_AHI_ANU_LSTv1.0_AusSubset.tif')

            # solar zenith angle
            zenith_angle = paste0(path2angle, format(Dates[k], '%Y/%m/%d/0'), t, '00/', 
                                  format(Dates[k], '%Y%m%d0'), t, '0000-P1S-ABOM_GEOM_SOLAR-PRJ_GEOS141_2000-HIMAWARI8-AHI.nc')

            if (file.exists(anu_lst) & file.exists(zenith_angle)){
                
                anu_lst = raster(anu_lst)
                zen_ang = raster(zenith_angle, varname='solar_zenith_angle')
                zen_ang = projectRaster(zen_ang, tem, method='ngb')
                
                # the modis utc time; will be used as mask to extract himawari-8 and modis lst data
                selected_time = modtime
                selected_time[selected_time < t-0.5 | selected_time > t+0.5] = NA

                # extract himawari-8 and modis lst data
                anu_lst = mask(anu_lst, selected_time)
                mod_lst = mask(rst_lst, selected_time)
                zen_ang = mask(zen_ang, selected_time)

            } else {

                anu_lst = tem
                mod_lst = tem
                zen_ang = tem
            }

            # save the data
            writeRaster(anu_lst, paste0(outpath, 'ANU/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '00_AHI_ANU_LSTv1.0_AusSubset.tif'), overwrite=TRUE)
            writeRaster(mod_lst, paste0(outpath, 'MODIS/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '00_MODIS_LST_AusSubset.tif'), overwrite=TRUE)
            writeRaster(zen_ang, paste0(outpath, 'SZA/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '00_Himawari_SolarZenithAngle_AusSubset.tif'), overwrite=TRUE)

        }
    }
}
