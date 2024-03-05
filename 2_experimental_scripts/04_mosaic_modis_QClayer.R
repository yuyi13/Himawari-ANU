# ################################################################
# objective:   mosaic modis quality control masks
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

# MODIS LST QA flag
# source: https://www.r-bloggers.com/2012/12/modis-qc-bits/

number2binary = function(number, noBits) {
    binary_vector = rev(as.numeric(intToBits(number)))
    if(missing(noBits)) {
        return(binary_vector)
    } else {
        binary_vector[-(1:(length(binary_vector) - noBits))]
    }
}

name_list = c('MOD11A1', 'MYD11A1')

# outpath
outpath = '/g/data/dt1/SatelliteLST/MODIS_LST/'

for (m in 1:2){

	# study period
	if (m == 1){
		Dates = seq(as.Date('2015-01-01'),as.Date('2021-12-31'),by='day')
	} else {
		Dates = seq(as.Date('2015-01-01'),as.Date('2022-12-31'),by='day')
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

	output_lst_qc = paste0(outpath, name_list[m], format(Dates[k], '/QualityControled_LST/%Y/'))
	if (!dir.exists(output_lst_qc)) dir.create(output_lst_qc, recursive=TRUE)

	output_qc = paste0(outpath, name_list[m], format(Dates[k], '/QC_flag/%Y/'))
	if (!dir.exists(output_qc)) dir.create(output_qc, recursive=TRUE)

	if (length(fl) > 0){

	lst_list = c()
	qc_list = c()
	for (i in 1:length(fl)){
		print(i)
		lst = try(raster(get_subdatasets(fl[i])[1]))
		qc  = try(raster(get_subdatasets(fl[i])[2]))
		lst_list = c(lst_list, lst)
		qc_list = c(qc_list, qc)
	}
	half1 = lst_list[1:10]; half1$fun='mean'; aus_lst1 = do.call(mosaic, half1)
	half2 = lst_list[11:length(lst_list)]; half2$fun='mean'; aus_lst2 = do.call(mosaic, half2)

	qc1 = qc_list[1:10]; qc1$fun='mean'; aus_qc1 = do.call(mosaic, qc1)
	qc2 = qc_list[11:length(qc_list)]; qc2$fun='mean'; aus_qc2 = do.call(mosaic, qc2)

	ll = mosaic(aus_lst1, aus_lst2, fun='mean'); ll = ll * 0.02
	qq = mosaic(aus_qc1, aus_qc2, fun='mean')

	# source: https://www.r-bloggers.com/2012/12/modis-qc-bits/
	# we only want pixels with values of 0, 5, 17, 21
	# 0  (in bit field): 0 0 0 0 0 0 0 0; LST produced, good quality, good data, emis err < 0.01, LST error <= 1K
	# 5  (in bit field): 0 0 0 0 0 1 0 1; LST produced, other quality, other quality, emis err < 0.01, LST error <= 1K
	# 17 (in bit field): 0 0 0 1 0 0 0 1; LST produced, other quality, good data, emis err < 0.02, LST error <= 1K
	# 21 (in bit field): 0 0 0 1 0 1 0 1; LST produced, other quality, other quality, emis err < 0.02, LST error <= 1K

	qq[qq != 0 & qq != 5 & qq != 17 & qq != 21] = NA

	ll = mask(ll, qq)

	ll = projectRaster(ll, tem, method='ngb',
		 filename=paste0(output_lst_qc, format(Dates[k], '%Y%m%d_'), name_list[m], '_QualityControled_LST_AusSubset.tif'), overwrite=TRUE)

	qq = projectRaster(qq, tem, method='ngb',
		 filename=paste0(output_qc, format(Dates[k], '%Y%m%d_'), name_list[m], '_QC_AusSubset.tif'), overwrite=TRUE)

	} else {
		writeRaster(tem, paste0(output_lst_qc, format(Dates[k], '%Y%m%d_'), name_list[m], '_QualityControled_LST_AusSubset.tif'), overwrite=TRUE)
		writeRaster(tem, paste0(output_qc, format(Dates[k], '%Y%m%d_'), name_list[m], '_QC_AusSubset.tif'), overwrite=TRUE)
	}
}
}
