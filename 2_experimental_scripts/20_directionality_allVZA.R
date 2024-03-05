# ################################################################
# objective:   directionality analysis using himawari-8 lst  
#              and viirs lst using all available vza
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

# references:
# Vinnikov, K. Y., Yu, Y., Goldberg, M. D., Tarpley, D., Romanov, P., Laszlo, I. and Chen, M., 2012. 
# Angular anisotropy of satellite observations of land surface temperature. Geophys. Res. Lett., 39. https://doi.org/10.1029/2012GL054059

# Ermida, S. L., DaCamara, C. C., Trigo, I. F., Pires, A. C., Ghent, D. and Remedios, J., 2017. 
# Modelling directional effects on remotely sensed land surface temperature. Remote Sens. Environ., 190, 56-69. https://doi.org/10.1016/j.rse.2016.12.008

source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_SetupForGraphics.R')
library(terra)
library(foreach)
library(doParallel)

#cl <- makeCluster(12)
#print(cl)
#registerDoParallel(cl)

# define the functions for the kernels of lst directionality analysis

# several points summarised from Ermida et al. (2017):
# 1. temporal match-up should have a time gap less than 10-min (they used 7.5-min) (we used 30-min window to be consistent with our study)
# 2. only pixels with vza below 50° should be considered for the bias correction (we did not do this in this version)
# 3. vza difference between two platforms should be less than 10° (they used 5 °) (we did not do this in this version)
# 4. do a linear bias correction to correct geo lst (h8) to polar orbit lst (viirs) (we did not do this and gave discussion)

vinnikov_kernel_model = function(par, geo_lst, geo_vza, geo_vaa, polar_lst, polar_vza, polar_vaa, sza, saa){

    # calculate relative azimuth angle
    # reference: https://stcorp.github.io/harp/doc/html/algorithms/derivations/relative_azimuth_angle.html
    geo_relative_aa = saa - geo_vaa
    geo_relative_aa[which(geo_relative_aa < 0)] = geo_relative_aa[which(geo_relative_aa < 0)] + 360
    geo_relative_aa[which(geo_relative_aa > 360)] = geo_relative_aa[which(geo_relative_aa > 360)] - 360

    polar_relative_aa = saa - polar_vaa
    polar_relative_aa[which(polar_relative_aa < 0)] = polar_relative_aa[which(polar_relative_aa < 0)] + 360
    polar_relative_aa[which(polar_relative_aa > 360)] = polar_relative_aa[which(polar_relative_aa > 360)] - 360

    geo_relative_aa = abs(geo_relative_aa - 180)
    polar_relative_aa = abs(polar_relative_aa - 180)

    # kernels for geo platform
    geo_emis_kernel  = 1 - cos(geo_vza * pi/180)
    geo_solar_kernel = sin(geo_vza * pi/180) * cos(sza * pi/180) * sin(sza * pi/180) * cos((sza - geo_vza) * pi/180) * cos(geo_relative_aa * pi/180)
    
    # kernels for polar platform
    polar_emis_kernel = 1 - cos(polar_vza * pi/180)
    polar_solar_kernel = sin(polar_vza * pi/180) * cos(sza * pi/180) * sin(sza * pi/180) * cos((sza - polar_vza) * pi/180) * cos(polar_relative_aa * pi/180)

    # objective function to optimise par A (par[1]) and D (par[2])
    OBJFUN = (geo_lst / polar_lst) - ((1 + par[1] * geo_emis_kernel + par[2] * geo_solar_kernel) / (1 + par[1] * polar_emis_kernel + par[2] * polar_solar_kernel))

    return(sum(OBJFUN^2))
}

ermida_implementation = function(geo_lst_stack, geo_vza_rst, geo_vaa_rst, 
                                polar_lst_stack, polar_vza_stack, polar_vaa_stack,
                                sza_stack, saa_stack){

    # prepare raster stacks for data
    geo_lst_array = as.array(geo_lst_stack)
    geo_vza_array = as.array(geo_vza_rst)
    geo_vaa_array = as.array(geo_vaa_rst)

    polar_lst_array = as.array(polar_lst_stack)
    polar_vza_array = as.array(polar_vza_stack)
    polar_vaa_array = as.array(polar_vaa_stack)

    sza_array = as.array(sza_stack)
    saa_array = as.array(saa_stack)

    # get the dimensions of the arrays
    NROW = dim(geo_lst_array)[1]; NCOL = dim(geo_lst_array)[2]

    # create empty matrices to store the parameters
    par_a_mtx = matrix(NA, nrow=NROW, ncol=NCOL)
    par_d_mtx = matrix(NA, nrow=NROW, ncol=NCOL)

    # pixelwise loop to optimise the pars
    for (x in 1:NROW){

        print(paste0('start to do the row ', x))

        for (y in 1:NCOL){

            # if valid sample number is less than 100, then set the parameters as NA
            if (length(na.omit(geo_lst_array[x,y,])) < 100) {

                par_a_mtx[x,y] = NA; par_d_mtx[x,y] = NA

            } else {

                df = data.frame(geo_lst = geo_lst_array[x,y,],
                                geo_vza = rep(geo_vza_array[x,y,], length(geo_lst_array[x,y,])), # geo platform has fixed VZA
                                geo_vaa = rep(geo_vaa_array[x,y,], length(geo_lst_array[x,y,])), # geo platform has fixed VAA
                                polar_lst = polar_lst_array[x,y,],
                                polar_vza = polar_vza_array[x,y,],
                                polar_vaa = polar_vaa_array[x,y,],
                                sza = sza_array[x,y,],
                                saa = saa_array[x,y,])
                
                sub_df = na.omit(df)
                
                # drop the rows with large lst diff between two time series
                # this is the place where we have difference with the version 'minVZAdiff'
                dropped_index = which(abs(sub_df$geo_lst - sub_df$polar_lst) > 10)
                
                # the dropped index can be integer(0) if there is no row to be dropped
                if (length(dropped_index) > 0) sub_df = sub_df[-dropped_index,]

                # use approximate values from Ermina et al. (2017) as initial values
                OPTIMRESULT = try(optim(par = c(-0.01, 0.02), fn = vinnikov_kernel_model, 
                                geo_lst = sub_df$geo_lst,
                                geo_vza = sub_df$geo_vza,
                                geo_vaa = sub_df$geo_vaa,
                                polar_lst = sub_df$polar_lst,
                                polar_vza = sub_df$polar_vza,
                                polar_vaa = sub_df$polar_vaa,
                                sza = sub_df$sza,
                                saa = sub_df$saa,
                                method = 'Nelder-Mead'))

                if (class(OPTIMRESULT) == 'try-error') OPTIMRESULT = list(par = c(-0.01, 0.02))

                par_a_mtx[x,y] = OPTIMRESULT$par[1]; par_d_mtx[x,y] = OPTIMRESULT$par[2]
            }
        }
    }

    par_a_rst = raster(par_a_mtx, crs=crs(geo_lst_stack), xmn=xmin(geo_lst_stack), xmx=xmax(geo_lst_stack),
                        ymn = ymin(geo_lst_stack), ymx = ymax(geo_lst_stack))
    par_d_rst = raster(par_d_mtx, crs=crs(geo_lst_stack), xmn=xmin(geo_lst_stack), xmx=xmax(geo_lst_stack),
                        ymn = ymin(geo_lst_stack), ymx = ymax(geo_lst_stack))

    return(list(par_a_rst, par_d_rst))
}

# paths to different data
path2baseline   = '/datasets/work/d61-af-soilmoisture/work/SatelliteLST/ANU_baseline_masked/'
path2chiba      = '/datasets/work/d61-af-soilmoisture/work/SatelliteLST/Chiba/AusSubset/'
path2copernicus = '/datasets/work/d61-af-soilmoisture/work/SatelliteLST/Copernicus/AusSubset_2km/'
path2anu_calib  = '/datasets/work/d61-af-soilmoisture/work/SatelliteLST/ANU_calib_masked/'

path2vii_lst    = '/datasets/work/d61-af-soilmoisture/work/himawari/hourly_comparison/VIIRS/LST/'
path2vii_vza    = '/datasets/work/d61-af-soilmoisture/work/himawari/hourly_comparison/VIIRS/VZA/'
path2vii_vaa    = '/datasets/work/d61-af-soilmoisture/work/himawari/hourly_comparison/VIIRS/VAA/'

path2sza        = '/datasets/work/d61-af-soilmoisture/work/SatelliteLST/Himawari_SZA/'
path2saa        = '/datasets/work/d61-af-soilmoisture/work/SatelliteLST/Himawari_SAA/'

# Time of interests
Dates = seq(as.Date('2016-01-01'),as.Date('2020-12-31'),by='day')
time_range = 3:7 # the approximate time range (GMT) of VIIRS overpass

# read lst, sza and vza data
baseline_list   = c()
chiba_list      = c()
copernicus_list = c()
anu_calib_list  = c()

viirs_lst_list  = c()
viirs_vza_list  = c()
viirs_vaa_list  = c()

sza_list        = c()
saa_list        = c()

for (k in 1:length(Dates)){

    # different himawari products
    baseline_lst   = paste0(path2baseline, format(Dates[k], '%Y/%m/%d/%Y%m%d0'), time_range, '0000_AHI_ANU_LSTv1.0_AusSubset.tif')
    chiba_lst      = paste0(path2chiba, format(Dates[k], '%Y/%m/%d/%Y%m%d0'), time_range, '00_AHI_Chiba_LSTv0_AusSubset.tif')
    copernicus_lst = paste0(path2copernicus, format(Dates[k], '%Y/%m/%d/%Y%m%d0'), time_range, '00_C_GLS_GEO_LSTv1.2.1_resampled_2km_AusSubset.tif')
    anu_calib_lst  = paste0(path2anu_calib, format(Dates[k], '%Y/%m/%d/%Y%m%d0'), time_range, '0000_AHI_ANU_LSTv1.4.1_AusSubset.tif')

    baseline_list  = c(baseline_list, baseline_lst)
    chiba_list     = c(chiba_list, chiba_lst)
    copernicus_list= c(copernicus_list, copernicus_lst)
    anu_calib_list = c(anu_calib_list, anu_calib_lst)

    # viirs lst and vza
    viirs_lst      = paste0(path2vii_lst, format(Dates[k], '%Y/%m/%d/%Y%m%d0'), time_range, '00_VNP21A1D_LST_AusSubset.tif')
    viirs_vza      = paste0(path2vii_vza, format(Dates[k], '%Y/%m/%d/%Y%m%d0'), time_range, '00_VNP09GA_VZA_AusSubset.tif')
    viirs_vaa      = paste0(path2vii_vaa, format(Dates[k], '%Y/%m/%d/%Y%m%d0'), time_range, '00_VNP09GA_VAA_AusSubset.tif')

    viirs_lst_list = c(viirs_lst_list, viirs_lst)
    viirs_vza_list = c(viirs_vza_list, viirs_vza)
    viirs_vaa_list = c(viirs_vaa_list, viirs_vaa)

    # solar angles
    sza            = paste0(path2sza, format(Dates[k], '%Y/%m/%d/%Y%m%d0'), time_range, '00_Himawari_SolarZenithAngle_AusSubset.tif')
    saa            = paste0(path2saa, format(Dates[k], '%Y/%m/%d/%Y%m%d0'), time_range, '00_Himawari_SolarAzimuthAngle_AusSubset.tif')

    sza_list       = c(sza_list, sza)
    saa_list       = c(saa_list, saa)
}

# australia grid template
aus_template = raster(extent(112, 154, -45, -10), res=0.02, crs=PROJ_LATLON)
#copernicus_template = raster(copernicus_list[1]); copernicus_template[] = NA

# check if there are missing files
#for (i in 1:length(baseline_list)) if (!file.exists(baseline_list[i])) print(paste0(baseline_list[i], ' does not exist')); writeRaster(aus_template, filename=baseline_list[i], overwrite=TRUE)
#for (i in 1:length(chiba_list)) if (!file.exists(chiba_list[i])) print(paste0(chiba_list[i], ' does not exist')); writeRaster(aus_template, filename=chiba_list[i], overwrite=TRUE)
#for (i in 1:length(copernicus_list)) if (!file.exists(copernicus_list[i])) writeRaster(copernicus_template, filename=copernicus_list[i], overwrite=TRUE)
#for (i in 1:length(anu_calib_list)) if (!file.exists(anu_calib_list[i])) writeRaster(aus_template, filename=anu_calib_list[i], overwrite=TRUE)

#for (i in 1:length(sza_list)) if (!file.exists(sza_list[i])) print(paste0(sza_list[i], ' does not exist')); writeRaster(aus_template, filename=sza_list[i], overwrite=TRUE)
#for (i in 1:length(saa_list)) if (!file.exists(saa_list[i])) print(paste0(saa_list[i], ' does not exist')); writeRaster(aus_template, filename=saa_list[i], overwrite=TRUE)

#for (i in 1:length(viirs_lst_list)) if (!file.exists(viirs_lst_list[i])) print(paste0(viirs_lst_list[i], ' does not exist')); writeRaster(aus_template, filename=viirs_lst_list[i], overwrite=TRUE)
#for (i in 1:length(viirs_vza_list)) if (!file.exists(viirs_vza_list[i])) print(paste0(viirs_lst_list[i], ' does not exist')); writeRaster(aus_template, filename=viirs_vza_list[i], overwrite=TRUE)
#for (i in 1:length(viirs_vaa_list)) if (!file.exists(viirs_vaa_list[i])) print(paste0(viirs_lst_list[i], ' does not exist')); writeRaster(aus_template, filename=viirs_vaa_list[i], overwrite=TRUE)

# raster is too big and we want to divide it into tiles
patch_long = 250 # patch size is 250 km * 250 km in this case
nl = orig_nl = nrow(aus_template); ns = orig_ns = ncol(aus_template)

n_nl = as.integer(orig_nl / patch_long); if (n_nl * patch_long < orig_nl) n_nl = n_nl + 1
n_ns = as.integer(orig_ns / patch_long); if (n_ns * patch_long < orig_ns) n_ns = n_ns + 1

# index of patches
ind_patch = matrix(NA, n_nl * n_ns, 4) # col, col, row, row

# get the edge indices for different patches
for (i_ns in 1:n_ns){
    for (i_nl in 1:n_nl){
        ind_patch[n_ns * (i_nl-1) + i_ns, 1] = (i_ns-1) * patch_long + 1
        ind_patch[n_ns * (i_nl-1) + i_ns, 2] = min(c(ns, i_ns * patch_long))
        ind_patch[n_ns * (i_nl-1) + i_ns, 3] = (i_nl-1) * patch_long + 1
        ind_patch[n_ns * (i_nl-1) + i_ns, 4] = min(c(nl, i_nl * patch_long))
    }
}

print('------ Start to read stacks ------')

# himawari-8 lst stack
# set variable from command line
geo_var = commandArgs(trailingOnly=TRUE)[1]

if (geo_var == 'baseline') {
    h8_lst_stack = rast(baseline_list)
} else if (geo_var == 'chiba') {
    h8_lst_stack = rast(chiba_list)
} else if (geo_var == 'copernicus') {
    h8_lst_stack = rast(copernicus_list)
} else if (geo_var == 'anu_calib') {
    h8_lst_stack = rast(anu_calib_list)
}

# himawari-8 vza raster
h8_vza_rst = raster('/datasets/work/d61-af-soilmoisture/work/himawari/ancillary/20150127000000-P1S-ABOM_GEOM_SENSOR-PRJ_GEOS141_2000-HIMAWARI8-AHI.nc', varname='sensor_zenith_angle')
h8_vaa_rst = raster('/datasets/work/d61-af-soilmoisture/work/himawari/ancillary/20150127000000-P1S-ABOM_GEOM_SENSOR-PRJ_GEOS141_2000-HIMAWARI8-AHI.nc', varname='sensor_azimuth_angle')
h8_vza_rst = rast(projectRaster(h8_vza_rst, aus_template, method='ngb'))
h8_vaa_rst = rast(projectRaster(h8_vaa_rst, aus_template, method='ngb'))

# viirs lst and vza stacks
viirs_lst_stack = rast(viirs_lst_list)
viirs_vza_stack = rast(viirs_vza_list)
viirs_vaa_stack = rast(viirs_vaa_list)

# solar angles
sza_stack = rast(sza_list) 
saa_stack = rast(saa_list)

print('------ The stack reading is finished ------')

# specify the output path
outpath = paste0('/datasets/work/d61-af-soilmoisture/work/himawari/directionality_par/Nelder_Mead_allVZA/', geo_var)
if (!dir.exists(paste0(outpath, '/par_a'))) dir.create(paste0(outpath, '/par_a'), recursive=TRUE)
if (!dir.exists(paste0(outpath, '/par_d'))) dir.create(paste0(outpath, '/par_d'), recursive=TRUE)

# loop through the tiles
foreach (t = 1:63, .combine=cbind, .packages=c('terra', 'raster')) %dopar% { # nrow is the number of tiles, which is 63 for now

    print(paste0('processing the tile ', t))

    ext_patch = ext(xmin(aus_template) + 0.02 * (ind_patch[t,1] - 1), 
                    xmin(aus_template) + 0.02 * (ind_patch[t,2]), 
                    ymax(aus_template) - 0.02 * (ind_patch[t,4]), 
                    ymax(aus_template) - 0.02 * (ind_patch[t,3] - 1))

    h8_lst_tile = crop(h8_lst_stack, ext_patch)
    h8_vza_tile = crop(h8_vza_rst, ext_patch)
    h8_vaa_tile = crop(h8_vaa_rst, ext_patch)

    viirs_lst_tile = crop(viirs_lst_stack, ext_patch)
    viirs_vza_tile = crop(viirs_vza_stack, ext_patch)
    viirs_vaa_tile = crop(viirs_vaa_stack, ext_patch)

    sza_tile = crop(sza_stack, ext_patch)
    saa_tile = crop(saa_stack, ext_patch)
    
    kernel_par_list = ermida_implementation(h8_lst_tile, h8_vza_tile, h8_vaa_tile, viirs_lst_tile, viirs_vza_tile, viirs_vaa_tile, sza_tile, saa_tile)

    writeRaster(kernel_par_list[[1]], filename = paste0(outpath, '/par_a/par_a_tile_', t, '.tif'), overwrite=TRUE)
    writeRaster(kernel_par_list[[2]], filename = paste0(outpath, '/par_d/par_d_tile_', t, '.tif'), overwrite=TRUE)
}

# mosaic all tiles

par_a_fl = list.files(paste0(outpath, '/par_a'), full.names=TRUE, pattern='.tif$')
par_d_fl = list.files(paste0(outpath, '/par_d'), full.names=TRUE, pattern='.tif$')

par_a_list = c(); par_d_list = c()

for (k in 1:length(par_a_fl)){
    rst1 = rast(par_a_fl[k]); rst2 = rast(par_d_fl[k])
    par_a_list = c(par_a_list, rst1); par_d_list = c(par_d_list, rst2)
}

par_a_list$fun='mean'; par_a_mosaic = do.call(mosaic, par_a_list) 
par_d_list$fun='mean'; par_d_mosaic = do.call(mosaic, par_d_list)

writeRaster(par_a_mosaic, filename = paste0(outpath, '/par_a_mosaic.tif'), overwrite=TRUE)
writeRaster(par_d_mosaic, filename = paste0(outpath, '/par_d_mosaic.tif'), overwrite=TRUE)

# find out the values at 1% and 99% quantiles
par_a_mosaic = raster(paste0(outpath, '/par_a_mosaic.tif'))
par_d_mosaic = raster(paste0(outpath, '/par_d_mosaic.tif'))

par_a_quant = quantile(par_a_mosaic, c(.01, .99))
par_d_quant = quantile(par_d_mosaic, c(.01, .99))

# mask out all the outliers
par_a_mosaic[par_a_mosaic < par_a_quant[1]] = par_a_quant[1]; par_a_mosaic[par_a_mosaic > par_a_quant[2]] = par_a_quant[2]
par_d_mosaic[par_d_mosaic < par_d_quant[1]] = par_d_quant[1]; par_d_mosaic[par_d_mosaic > par_d_quant[2]] = par_d_quant[2]

writeRaster(par_a_mosaic, filename = paste0(outpath, '/par_a_mosaic_1to99percentile.tif'), overwrite=TRUE)
writeRaster(par_d_mosaic, filename = paste0(outpath, '/par_d_mosaic_1to99percentile.tif'), overwrite=TRUE)
