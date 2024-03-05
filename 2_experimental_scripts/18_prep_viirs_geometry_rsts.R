# ################################################################
# objective:   prepare viirs geometry for directionality analysis
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
        path2time = paste0(path2vii, format(Dates[k], 'VNP21A1D_Time/%Y/'))
        path2vza  = paste0(path2vii, format(Dates[k], 'VNP09GA_VZA/%Y/'))
        path2vaa  = paste0(path2vii, format(Dates[k], 'VNP09GA_VAA/%Y/'))
        path2sza  = paste0(path2vii, format(Dates[k], 'VNP09GA_SZA/%Y/'))
        path2saa  = paste0(path2vii, format(Dates[k], 'VNP09GA_SAA/%Y/'))

        # find the files
        time = paste0(path2time, format(Dates[k], '%Y%m%d_'), 'VNP21A1D_Time_AusSubset.tif')
        vza  = paste0(path2vza,  format(Dates[k], '%Y%m%d_'), 'VNP09GA_VZA_AusSubset.tif')
        vaa  = paste0(path2vaa,  format(Dates[k], '%Y%m%d_'), 'VNP09GA_VAA_AusSubset.tif')
        sza  = paste0(path2sza,  format(Dates[k], '%Y%m%d_'), 'VNP09GA_SZA_AusSubset.tif')
        saa  = paste0(path2saa,  format(Dates[k], '%Y%m%d_'), 'VNP09GA_SAA_AusSubset.tif')

        if (file.exists(time)){

            # read data as raster
            rst_time = raster(time)
            rst_vza = raster(vza)
            rst_vaa = raster(vaa)
            rst_sza = raster(sza)
            rst_saa = raster(saa)

            rst_time = projectRaster(rst_time, tem, method='ngb')
            rst_vza = projectRaster(rst_vza, tem, method='ngb')
            rst_vaa = projectRaster(rst_vaa, tem, method='ngb')
            rst_sza = projectRaster(rst_sza, tem, method='ngb')
            rst_saa = projectRaster(rst_saa, tem, method='ngb')
        
            # convert viirs time to utc
            viitime = rst_time - lon_rst/15

        } else {

            viitime = tem
            rst_vza = tem
            rst_vaa = tem
            rst_sza = tem
            rst_saa = tem
        }

        # outpath 
        outpath_vza = paste0(outpath, 'VIIRS/VZA/', format(Dates[k], '%Y/%m/%d/'))
        if (!dir.exists(outpath_vza)) dir.create(outpath_vza, recursive=TRUE)

        outpath_vaa = paste0(outpath, 'VIIRS/VAA/', format(Dates[k], '%Y/%m/%d/'))
        if (!dir.exists(outpath_vaa)) dir.create(outpath_vaa, recursive=TRUE)

        outpath_sza = paste0(outpath, 'VIIRS/SZA/', format(Dates[k], '%Y/%m/%d/'))
        if (!dir.exists(outpath_sza)) dir.create(outpath_sza, recursive=TRUE)

        outpath_saa = paste0(outpath, 'VIIRS/SAA/', format(Dates[k], '%Y/%m/%d/'))
        if (!dir.exists(outpath_saa)) dir.create(outpath_saa, recursive=TRUE)

        for (t in time_range){
            
            # the viirs utc time; will be used as mask to extract viirs lst and vza data
            selected_time = viitime
            selected_time[selected_time < t-0.5 | selected_time > t+0.5] = NA

            # extract viirs viewing geometry data
            vii_vza = mask(rst_vza, selected_time)
            vii_vaa = mask(rst_vaa, selected_time)
            vii_sza = mask(rst_sza, selected_time)
            vii_saa = mask(rst_saa, selected_time)

            # save the data
            writeRaster(vii_vza, paste0(outpath, 'VIIRS/VZA/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '00_VNP09GA_VZA_AusSubset.tif'), overwrite=TRUE)
            writeRaster(vii_vaa, paste0(outpath, 'VIIRS/VAA/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '00_VNP09GA_VAA_AusSubset.tif'), overwrite=TRUE)
            writeRaster(vii_sza, paste0(outpath, 'VIIRS/SZA/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '00_VNP09GA_SZA_AusSubset.tif'), overwrite=TRUE)
            writeRaster(vii_saa, paste0(outpath, 'VIIRS/SAA/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), t, '00_VNP09GA_SAA_AusSubset.tif'), overwrite=TRUE)
        }
    }
