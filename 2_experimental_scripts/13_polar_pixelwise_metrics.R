# #################################################################
# objective:   use terra package to calculate pixelwise metrics 
#              against polar-orbiting lst
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
###################################################################

source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_SetupForGraphics.R')
library(terra)
library(foreach)
library(doParallel)
cl <- makeCluster(48)
print(cl)
registerDoParallel(cl)

path2data = '/datasets/work/d61-af-soilmoisture/work/himawari/Diff/'
out_path = '/datasets/work/d61-af-soilmoisture/work/himawari/pixelwise_metrics/'

aus_template = raster(extent(112, 154, -45, -10), res=0.02, crs=PROJ_LATLON)

count_pixels = function(x) {
  return(length(x[!is.na(x)]))
}

#########################################
# calculate metrics for MODIS best pixels
#########################################

anu_diff_list = list.files(paste0(path2data, 'ANU-MODISbest'), full.names=TRUE)
anucalib_diff_list = list.files(paste0(path2data, 'ANUcalib-MODISbest'), full.names=TRUE)

print('-------------------------------------------------')
print('| Start to prepare stacks for MODIS best pixels |')
print('-------------------------------------------------')

anu_diff_stack = rast(anu_diff_list)
anucalib_diff_stack = rast(anucalib_diff_list)

print('Start to calculate metrics')


anu_mod_bias = app(anu_diff_stack, fun='mean', na.rm=TRUE, cores=cl)
anucalib_mod_bias = app(anucalib_diff_stack, fun='mean', na.rm=TRUE, cores=cl)

writeRaster(anu_mod_bias, paste0(out_path, 'ANU_minus_MODISbest_bias_2016_2020.tif'), overwrite=TRUE)
writeRaster(anucalib_mod_bias, paste0(out_path, 'ANUcalib_minus_MODISbest_bias_2016_2020.tif'), overwrite=TRUE)

anu_mod_ubRMSE = app(anu_diff_stack, fun='sd', na.rm=TRUE, cores=cl)
anucalib_mod_ubRMSE = app(anucalib_diff_stack, fun='sd', na.rm=TRUE, cores=cl)

writeRaster(anu_mod_ubRMSE, paste0(out_path, 'ANU_minus_MODISbest_ubRMSE_2016_2020.tif'), overwrite=TRUE)
writeRaster(anucalib_mod_ubRMSE, paste0(out_path, 'ANUcalib_minus_MODISbest_ubRMSE_2016_2020.tif'), overwrite=TRUE)


anu_valid_pixels = app(anu_diff_stack, fun=count_pixels, cores=cl)
anucalib_valid_pixels = app(anucalib_diff_stack, fun=count_pixels, cores=cl)

writeRaster(anu_valid_pixels, paste0(out_path, 'ANU_minus_MODISbest_valid_pixels_2016_2020.tif'), overwrite=TRUE)
writeRaster(anucalib_valid_pixels, paste0(out_path, 'ANUcalib_minus_MODISbest_valid_pixels_2016_2020.tif'), overwrite=TRUE)

rm(anu_diff_stack, anucalib_diff_stack)

print('Finished metrics for MODISbest during 2016 - 2020')
print(' ')

###################################
# calculate metrics for MODIS/Terra
###################################

anu_diff_list   = list.files(paste0(path2data, 'ANU-MOD11A1all'), full.names=TRUE)
anucalib_diff_list = list.files(paste0(path2data, 'ANUcalib-MOD11A1all'), full.names=TRUE)
chiba_diff_list = list.files(paste0(path2data, 'Chiba-MOD11A1all'), full.names=TRUE)
cop_diff_list   = list.files(paste0(path2data, 'Copernicus-MOD11A1all'), full.names=TRUE)

print('----------------------------------------------')
print('| Start to prepare stacks for MOD11A1 pixels |')
print('----------------------------------------------')

anu_diff_stack = rast(anu_diff_list)
anucalib_diff_stack = rast(anucalib_diff_list)
chiba_diff_stack = rast(chiba_diff_list)
cop_diff_stack = rast(cop_diff_list)

print('Start to calculate metrics')

anu_mod_bias = app(anu_diff_stack, fun='mean', na.rm=TRUE, cores=cl)
anucalib_mod_bias = app(anucalib_diff_stack, fun='mean', na.rm=TRUE, cores=cl)
chiba_mod_bias = app(chiba_diff_stack, fun='mean', na.rm=TRUE, cores=cl)
cop_mod_bias = app(cop_diff_stack, fun='mean', na.rm=TRUE, cores=cl)

writeRaster(anu_mod_bias, paste0(out_path, 'ANU_minus_MOD11A1_bias_2016_2020.tif'), overwrite=TRUE)
writeRaster(anucalib_mod_bias, paste0(out_path, 'ANUcalib_minus_MOD11A1_bias_2016_2020.tif'), overwrite=TRUE)
writeRaster(chiba_mod_bias, paste0(out_path, 'Chiba_minus_MOD11A1_bias_2016_2020.tif'), overwrite=TRUE)
writeRaster(cop_mod_bias, paste0(out_path, 'Copernicus_minus_MOD11A1_bias_2016_2020.tif'), overwrite=TRUE)

anu_mod_ubRMSE = app(anu_diff_stack, fun='sd', na.rm=TRUE, cores=cl)
anucalib_mod_ubRMSE = app(anucalib_diff_stack, fun='sd', na.rm=TRUE, cores=cl)
chiba_mod_ubRMSE = app(chiba_diff_stack, fun='sd', na.rm=TRUE, cores=cl)
cop_mod_ubRMSE = app(cop_diff_stack, fun='sd', na.rm=TRUE, cores=cl)

writeRaster(anu_mod_ubRMSE, paste0(out_path, 'ANU_minus_MOD11A1_ubRMSE_2016_2020.tif'), overwrite=TRUE)
writeRaster(anucalib_mod_ubRMSE, paste0(out_path, 'ANUcalib_minus_MOD11A1_ubRMSE_2016_2020.tif'), overwrite=TRUE)
writeRaster(chiba_mod_ubRMSE, paste0(out_path, 'Chiba_minus_MOD11A1_ubRMSE_2016_2020.tif'), overwrite=TRUE)
writeRaster(cop_mod_ubRMSE, paste0(out_path, 'Copernicus_minus_MOD11A1_ubRMSE_2016_2020.tif'), overwrite=TRUE)

anu_valid_pixels = app(anu_diff_stack, fun=count_pixels, cores=cl)
anucalib_valid_pixels = app(anucalib_diff_stack, fun=count_pixels, cores=cl)
chiba_valid_pixels = app(chiba_diff_stack, fun=count_pixels, cores=cl)
cop_valid_pixels = app(cop_diff_stack, fun=count_pixels, cores=cl)

writeRaster(anu_valid_pixels, paste0(out_path, 'ANU_minus_MOD11A1_valid_pixels_2016_2020.tif'), overwrite=TRUE)
writeRaster(anucalib_valid_pixels, paste0(out_path, 'ANUcalib_minus_MOD11A1_valid_pixels_2016_2020.tif'), overwrite=TRUE)
writeRaster(chiba_valid_pixels, paste0(out_path, 'Chiba_minus_MOD11A1_valid_pixels_2016_2020.tif'), overwrite=TRUE)
writeRaster(cop_valid_pixels, paste0(out_path, 'Copernicus_minus_MOD11A1_valid_pixels_2016_2020.tif'), overwrite=TRUE)

rm(anu_diff_stack, anucalib_diff_stack, chiba_diff_stack, cop_diff_stack)

print('Finished metrics for MOD11A1 during 2016 - 2020')
print(' ')

##################################
# calculate metrics for MODIS/Aqua
##################################

anu_diff_list   = list.files(paste0(path2data, 'ANU-MYD11A1all'), full.names=TRUE)
anucalib_diff_list = list.files(paste0(path2data, 'ANUcalib-MYD11A1all'), full.names=TRUE)
chiba_diff_list = list.files(paste0(path2data, 'Chiba-MYD11A1all'), full.names=TRUE)
cop_diff_list   = list.files(paste0(path2data, 'Copernicus-MYD11A1all'), full.names=TRUE)

print('----------------------------------------------')
print('| Start to prepare stacks for MYD11A1 pixels |')
print('----------------------------------------------')

anu_diff_stack = rast(anu_diff_list)
anucalib_diff_stack = rast(anucalib_diff_list)
chiba_diff_stack = rast(chiba_diff_list)
cop_diff_stack = rast(cop_diff_list)

print('Start to calculate metrics')

anu_mod_bias = app(anu_diff_stack, fun='mean', na.rm=TRUE, cores=cl)
anucalib_mod_bias = app(anucalib_diff_stack, fun='mean', na.rm=TRUE, cores=cl)
chiba_mod_bias = app(chiba_diff_stack, fun='mean', na.rm=TRUE, cores=cl)
cop_mod_bias = app(cop_diff_stack, fun='mean', na.rm=TRUE, cores=cl)

writeRaster(anu_mod_bias, paste0(out_path, 'ANU_minus_MYD11A1_bias_2016_2020.tif'), overwrite=TRUE)
writeRaster(anucalib_mod_bias, paste0(out_path, 'ANUcalib_minus_MYD11A1_bias_2016_2020.tif'), overwrite=TRUE)
writeRaster(chiba_mod_bias, paste0(out_path, 'Chiba_minus_MYD11A1_bias_2016_2020.tif'), overwrite=TRUE)
writeRaster(cop_mod_bias, paste0(out_path, 'Copernicus_minus_MYD11A1_bias_2016_2020.tif'), overwrite=TRUE)

anu_mod_ubRMSE = app(anu_diff_stack, fun='sd', na.rm=TRUE, cores=cl)
anucalib_mod_ubRMSE = app(anucalib_diff_stack, fun='sd', na.rm=TRUE, cores=cl)
chiba_mod_ubRMSE = app(chiba_diff_stack, fun='sd', na.rm=TRUE, cores=cl)
cop_mod_ubRMSE = app(cop_diff_stack, fun='sd', na.rm=TRUE, cores=cl)

writeRaster(anu_mod_ubRMSE, paste0(out_path, 'ANU_minus_MYD11A1_ubRMSE_2016_2020.tif'), overwrite=TRUE)
writeRaster(anucalib_mod_ubRMSE, paste0(out_path, 'ANUcalib_minus_MYD11A1_ubRMSE_2016_2020.tif'), overwrite=TRUE)
writeRaster(chiba_mod_ubRMSE, paste0(out_path, 'Chiba_minus_MYD11A1_ubRMSE_2016_2020.tif'), overwrite=TRUE)
writeRaster(cop_mod_ubRMSE, paste0(out_path, 'Copernicus_minus_MYD11A1_ubRMSE_2016_2020.tif'), overwrite=TRUE)

anu_valid_pixels = app(anu_diff_stack, fun=count_pixels, cores=cl)
anucalib_valid_pixels = app(anucalib_diff_stack, fun=count_pixels, cores=cl)
chiba_valid_pixels = app(chiba_diff_stack, fun=count_pixels, cores=cl)
cop_valid_pixels = app(cop_diff_stack, fun=count_pixels, cores=cl)

writeRaster(anu_valid_pixels, paste0(out_path, 'ANU_minus_MYD11A1_valid_pixels_2016_2020.tif'), overwrite=TRUE)
writeRaster(anucalib_valid_pixels, paste0(out_path, 'ANUcalib_minus_MYD11A1_valid_pixels_2016_2020.tif'), overwrite=TRUE)
writeRaster(chiba_valid_pixels, paste0(out_path, 'Chiba_minus_MYD11A1_valid_pixels_2016_2020.tif'), overwrite=TRUE)
writeRaster(cop_valid_pixels, paste0(out_path, 'Copernicus_minus_MYD11A1_valid_pixels_2016_2020.tif'), overwrite=TRUE)

rm(anu_diff_stack, anucalib_diff_stack, chiba_diff_stack, cop_diff_stack)

print('Finished metrics for MYD11A1 during 2016 - 2020')
print(' ')

#############################
# calculate metrics for VIIRS
#############################

anu_diff_list   = list.files(paste0(path2data, 'ANU-VIIRS'), full.names=TRUE)
anucalib_diff_list = list.files(paste0(path2data, 'ANUcalib-VIIRS'), full.names=TRUE)
chiba_diff_list = list.files(paste0(path2data, 'Chiba-VIIRS'), full.names=TRUE)
cop_diff_list   = list.files(paste0(path2data, 'Copernicus-VIIRS'), full.names=TRUE)

print('--------------------------------------------')
print('| Start to prepare stacks for VIIRS pixels |')
print('--------------------------------------------')

anu_diff_stack = rast(anu_diff_list)
anucalib_diff_stack = rast(anucalib_diff_list)
chiba_diff_stack = rast(chiba_diff_list)
cop_diff_stack = rast(cop_diff_list)

print('Start to calculate metrics')

anu_vii_bias = app(anu_diff_stack, fun='mean', na.rm=TRUE, cores=cl)
anucalib_vii_bias = app(anucalib_diff_stack, fun='mean', na.rm=TRUE, cores=cl)
chiba_vii_bias = app(chiba_diff_stack, fun='mean', na.rm=TRUE, cores=cl)
cop_vii_bias = app(cop_diff_stack, fun='mean', na.rm=TRUE, cores=cl)

writeRaster(anu_vii_bias, paste0(out_path, 'ANU_minus_VIIRS_bias_2016_2020.tif'), overwrite=TRUE)
writeRaster(anucalib_vii_bias, paste0(out_path, 'ANUcalib_minus_VIIRS_bias_2016_2020.tif'), overwrite=TRUE)
writeRaster(chiba_vii_bias, paste0(out_path, 'Chiba_minus_VIIRS_bias_2016_2020.tif'), overwrite=TRUE)
writeRaster(cop_vii_bias, paste0(out_path, 'Copernicus_minus_VIIRS_bias_2016_2020.tif'), overwrite=TRUE)

anu_vii_ubRMSE = app(anu_diff_stack, fun='sd', na.rm=TRUE, cores=cl)
anucalib_vii_ubRMSE = app(anucalib_diff_stack, fun='sd', na.rm=TRUE, cores=cl)
chiba_vii_ubRMSE = app(chiba_diff_stack, fun='sd', na.rm=TRUE, cores=cl)
cop_vii_ubRMSE = app(cop_diff_stack, fun='sd', na.rm=TRUE, cores=cl)

writeRaster(anu_vii_ubRMSE, paste0(out_path, 'ANU_minus_VIIRS_ubRMSE_2016_2020.tif'), overwrite=TRUE)
writeRaster(anucalib_vii_ubRMSE, paste0(out_path, 'ANUcalib_minus_VIIRS_ubRMSE_2016_2020.tif'), overwrite=TRUE)
writeRaster(chiba_vii_ubRMSE, paste0(out_path, 'Chiba_minus_VIIRS_ubRMSE_2016_2020.tif'), overwrite=TRUE)
writeRaster(cop_vii_ubRMSE, paste0(out_path, 'Copernicus_minus_VIIRS_ubRMSE_2016_2020.tif'), overwrite=TRUE)


anu_valid_pixels = app(anu_diff_stack, fun=count_pixels, cores=cl)
anucalib_valid_pixels = app(anucalib_diff_stack, fun=count_pixels, cores=cl)
chiba_valid_pixels = app(chiba_diff_stack, fun=count_pixels, cores=cl)
cop_valid_pixels = app(cop_diff_stack, fun=count_pixels, cores=cl)

writeRaster(anu_valid_pixels, paste0(out_path, 'ANU_minus_VIIRS_valid_pixels_2016_2020.tif'), overwrite=TRUE)
writeRaster(anucalib_valid_pixels, paste0(out_path, 'ANUcalib_minus_VIIRS_valid_pixels_2016_2020.tif'), overwrite=TRUE)
writeRaster(chiba_valid_pixels, paste0(out_path, 'Chiba_minus_VIIRS_valid_pixels_2016_2020.tif'), overwrite=TRUE)
writeRaster(cop_valid_pixels, paste0(out_path, 'Copernicus_minus_VIIRS_valid_pixels_2016_2020.tif'), overwrite=TRUE)

rm(anu_diff_stack, anucalib_diff_stack, chiba_diff_stack, cop_diff_stack)

print('Finished metrics for VIIRS during 2016 - 2020')
