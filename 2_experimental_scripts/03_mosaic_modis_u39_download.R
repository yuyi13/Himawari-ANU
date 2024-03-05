# ################################################################
# objective:   mosaic modis lst and time layer data
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_SetupForGraphics.R')
library(gdalUtils)
library(foreach)
library(doParallel)

cl <- makeCluster(24)
print(cl)
registerDoParallel(cl)

tem = raster(extent(112,154,-45,-10), res=0.01, crs=PROJ_LATLON)

name_list = c('MOD11A1', 'MYD11A1')

# outpath
outpath = '/g/data/dt1/SatelliteLST/MODIS_LST/'

for (m in 1:1){

	# study period
	if (m == 1){
		Dates = seq(as.Date('2013-01-01'),as.Date('2014-12-31'),by='day')
	} else {
		Dates = seq(as.Date('2013-01-01'),as.Date('2014-12-31'),by='day')
	}

foreach (k = 1:length(Dates), .packages=c('raster', 'gdalUtils', 'stringr')) %dopar% {

	if (Dates[k] < as.Date('2019-11-01')) {
		# read data from NCI u39
		path2modis = '/g/data/u39/public/data/modis/lpdaac-tiles-c6/'

		DOI = paste0(path2modis, name_list[m], '.006/', format(Dates[k], '%Y.%m.%d/'))
		print(DOI)
		fl = list.files(DOI, full.names=TRUE, pattern='.hdf$')

	} else {
		# read downloaded data from scratch 
		path2modis = paste0('/scratch/os22/yy4778/MODIS_data/', name_list[m])

		DOI = paste0(name_list[m], format(Dates[k], '.A%Y%j.h'))
		print(DOI)
		fl = list.files(path2modis, pattern=DOI, full.names=TRUE)
	}

	output_lst = paste0(outpath, name_list[m], format(Dates[k], '/LST/%Y/'))
	if (!dir.exists(output_lst)){dir.create(output_lst, recursive=TRUE)}

	output_time = paste0(outpath, name_list[m], format(Dates[k], '/Time/%Y/'))
	if (!dir.exists(output_time)){dir.create(output_time, recursive=TRUE)}

	if (length(fl) > 0){

		lst_list = c()
		time_list = c()
		for (i in 1:length(fl)){
			print(i)
			lst = try(raster(get_subdatasets(fl[i])[1]))
			view_time = try(raster(get_subdatasets(fl[i])[3]))
			lst_list = c(lst_list, lst)
			time_list = c(time_list, view_time)
		}

		if (length(lst_list) > 11){
			half1 = lst_list[1:10]; half1$fun='mean'; aus_lst1 = do.call(mosaic, half1)
			half2 = lst_list[11:length(lst_list)]; half2$fun='mean'; aus_lst2 = do.call(mosaic, half2)

			time1 = time_list[1:10]; time1$fun='mean'; aus_time1 = do.call(mosaic, time1)
			time2 = time_list[11:length(time_list)]; time2$fun='mean'; aus_time2 = do.call(mosaic, time2)

			ll = mosaic(aus_lst1, aus_lst2, fun='mean'); ll = ll * 0.02
			tt = mosaic(aus_time1, aus_time2, fun='mean'); tt = tt * 0.1

			ll = projectRaster(ll, tem, method='ngb',
				filename=paste0(output_lst, format(Dates[k], '%Y%m%d_'), name_list[m], '_LST_AusSubset.tif'), overwrite=TRUE)

			tt = projectRaster(tt, tem, method='ngb',
				filename=paste0(output_time, format(Dates[k], '%Y%m%d_'), name_list[m], '_Time_AusSubset.tif'), overwrite=TRUE)
		} else {
			
			writeRaster(tem, paste0(output_lst, format(Dates[k], '%Y%m%d_'), name_list[m], '_LST_AusSubset.tif'), overwrite=TRUE)
			writeRaster(tem, paste0(output_time, format(Dates[k], '%Y%m%d_'), name_list[m], '_Time_AusSubset.tif'), overwrite=TRUE)
		}
	
	} else {
		writeRaster(tem, paste0(output_lst, format(Dates[k], '%Y%m%d_'), name_list[m], '_LST_AusSubset.tif'), overwrite=TRUE)
		writeRaster(tem, paste0(output_time, format(Dates[k], '%Y%m%d_'), name_list[m], '_Time_AusSubset.tif'), overwrite=TRUE)
	}
	
}
}
