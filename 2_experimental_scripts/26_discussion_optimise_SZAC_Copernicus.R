# ################################################################
# objective:   optimise szac parameter using copernicus lst
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_SetupForGraphics.R')
library(terra)

Dates = seq(as.Date('2016-01-01'),as.Date('2020-12-31'),by='day')

processed_path = '/datasets/work/d61-af-soilmoisture/work/himawari/processed/'
output_path = '/datasets/work/d61-af-soilmoisture/work/himawari/Copernicus_ZAC/Brent/'

if (!dir.exists(paste0(output_path, '/par1'))) dir.create((paste0(output_path, '/par1')), recursive=TRUE)

aus_template = raster(extent(112, 154, -45, -10), res=0.02, crs=PROJ_LATLON)

time_range = 0:8

cop_list = c()
mod_list = c()
sza_list = c()

for (k in 1:length(Dates)){

    cop_lst = paste0(processed_path, 'Copernicus/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), time_range, '00_C_GLS_GEO_LSTv1.2.1_AusSubset.tif')
    mod_lst = paste0(processed_path, 'MODIS/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), time_range, '00_MODIS_LST_AusSubset.tif')
    sza     = paste0(processed_path, 'SZA/', format(Dates[k], '%Y/%m/%d/%Y%m%d0'), time_range, '00_Himawari_SolarZenithAngle_AusSubset.tif')

    cop_list = c(cop_list, cop_lst)
    mod_list = c(mod_list, mod_lst)
    sza_list = c(sza_list, sza)
}

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

# Objective function for ZAC
OBJFUN = function(par, lst_input, lst_ref, sol_ang){

    RMSE = function(m, o){
        sqrt(mean((m - o)^2, na.rm=TRUE))
    }

    cosSZA = cos(sol_ang * pi/180)
    zac = par[1] * log(cosSZA + 1)

    # zac represents the bias level, which is a function expressed in cosSZA
	lst_calib = lst_input - zac

    OBJ = RMSE(lst_calib, lst_ref)

    return(OBJ)
}

# solar zenith angle-based calibration
SZAC = function(first_stack, second_stack, angle_stack){

    # convert two rasterstacks to 3-D arrays; stacks should be in same dim
    first_array = as.array(first_stack)
    second_array = as.array(second_stack)
    angle_array = as.array(angle_stack)

    NROW = dim(first_array)[1]; NCOL = dim(first_array)[2]

    zacPar1_mtx = matrix(NA, nrow=NROW, ncol=NCOL)

    # pixelwise loop to optimise the pars against LST reference
    for (x in 1:NROW){

        print(paste0('start to do the row ', x))

        for (y in 1:NCOL){

            # if valid sample number is less than 100, then set the parameters as NA
            if (length(na.omit(first_array[x,y,])) < 100) {

                zacPar1_mtx[x,y] = NA
            } else {

                df = data.frame(lst_input = first_array[x,y,],
                                lst_ref = second_array[x,y,],
                                sol_ang = angle_array[x,y,])
                
                sub_df = na.omit(df)
                
                # drop the rows with too big difference between input and reference
                dropped_index = which(abs(sub_df$lst_input - sub_df$lst_ref) > 10)
                
                # the dropped index can be integer(0) if there is no row to be dropped
                if (length(dropped_index) > 0) sub_df = sub_df[-dropped_index,]
                sys_diff = sub_df$lst_input - sub_df$lst_ref

                med_value = abs(median(sys_diff))

                # use median value as an inital guess for the optimisation
                OPTIMRESULT = try(optim(par = med_value, fn = OBJFUN, 
                                lst_input = sub_df$lst_input,
                                lst_ref = sub_df$lst_ref, 
                                sol_ang = sub_df$sol_ang,
                                lower = (-med_value * 1.5), upper = (med_value * 1.5),
                                method = 'Brent'))

                if (class(OPTIMRESULT) == 'try-error') OPTIMRESULT = list(par = med_value)

                zacPar1_mtx[x,y] = OPTIMRESULT$par[1]
            }
        }
    }

    zacPar1_rst = raster(zacPar1_mtx, crs=crs(first_stack), xmn=xmin(first_stack), xmx=xmax(first_stack),
                        ymn = ymin(first_stack), ymx = ymax(first_stack))

    return(list(zacPar1_rst))
}

print('------ Start to do optimisation ------')

library(foreach)
library(doParallel)
#cl <- makeCluster(12)
#print(cl)
#registerDoParallel(cl)

print('------ Start to read stacks ------')
cop_stack = rast(cop_list) # himawari-8 lst to be calibrated
mod_stack = rast(mod_list) # modis lst as the reference
sza_stack = rast(sza_list) # solar zenith angle

print('------ The stack reading is finished ------')

foreach (t = 1:63, .combine=cbind, .packages=c('terra', 'raster')) %dopar% { # nrow is the number of tiles, which is 63 for now

    print(paste0('processing the tile ', t))

    ext_patch = ext(xmin(aus_template) + 0.02 * (ind_patch[t,1] - 1), 
                    xmin(aus_template) + 0.02 * (ind_patch[t,2]), 
                    ymax(aus_template) - 0.02 * (ind_patch[t,4]), 
                    ymax(aus_template) - 0.02 * (ind_patch[t,3] - 1))

    cop_tile = crop(cop_stack, ext_patch)
    mod_tile = crop(mod_stack, ext_patch)
    sza_tile = crop(sza_stack, ext_patch)

    parRst_list = SZAC(cop_tile, mod_tile, sza_tile)

    writeRaster(parRst_list[[1]], filename = paste0(output_path, 'par1/zacPar1_tile_', t, '.tif'), overwrite=TRUE)
}

## mosaic all tiles

par1_fl = list.files(paste0(output_path, 'par1'), full.names=TRUE, pattern='.tif$')

par1_list = c()

for (k in 1:length(par1_fl)){
    rst1 = rast(par1_fl[k])
    par1_list = c(par1_list, rst1)
}

par1_list$fun='mean'; par1_aus = do.call(mosaic, par1_list); 
writeRaster(par1_aus, paste0(output_path, 'SZAC_Copernicus_log_par1_2km.tif'), overwrite=TRUE)

par1_aus = raster(paste0(output_path, 'SZAC_Copernicus_log_par1_2km.tif'))

# find out the values at 1% and 99% quantiles
par1_quant = quantile(par1_aus, c(.01, .99))

# mask out all the outliers
par1_aus[par1_aus < par1_quant[1]] = par1_quant[1]; par1_aus[par1_aus > par1_quant[2]] = par1_quant[2]

writeRaster(par1_aus, paste0(output_path, 'SZAC_Copernicus_log_par1_2km_1to99percentile.tif'), overwrite=TRUE)
