### Baseline version developed by: Luigi Renzullo (email: luigi.renzullo@bom.gov.au)
### Version 1.4.1 developed and maintained by: Yi Yu (email: yi.yu1@anu.edu.au)
### Himawari-8 observations are availble until 13/Dec/2022
### Himawari-9 observations are availble since 27/Sep/2022

### libraries and modules
requiredPackages = c('raster', 'ncdf4')
for (k in requiredPackages){
    if (!require(k, character.only = TRUE)) install.packages(k, repos='https://cloud.r-project.org')
    library(k, character.only = TRUE)
}

### image dimensions for 2-km full-disk
NROWS = 5500; NCOLS = 5500

### projection information
PROJ_LATLON = '+proj=longlat +datum=WGS84'
PROJ_GEO = '+proj=geos +lon_0=140.7 +h=35785863 +a=6378137.0 +b=6356752.3'
Emn = -5500000; Emx = 5500000; Nmn = -5500000; Nmx = 5500000

### reading Date from the command line
### date must be in format 'YYYY-mm-dd', e.g., '2016-01-01'
args = commandArgs(trailingOnly=TRUE)
DOI  = as.Date(args)
print(paste('========= DateString =',args))
yyyy   = format(DOI,  '%Y')
mm     = format(DOI,  '%m')
dd     = format(DOI,  '%d') 

print('+--------------------------------------------------------------------------------+')
print('+--------------------------------------------------------------------------------+')
print('+                          H I M A W A R I   A H I                               +')
print('+     L a n d   S u r f a c e   T e m p e r a t u r e   r e t r i e v a l s      +')
print(paste0('+                            f o r ',DOI,'                                    +'))
print('+--------------------------------------------------------------------------------+')
print('+--------------------------------------------------------------------------------+')
print('')

### directory paths
TB_DirPath = SOL_DirPath = CMA_DirPath = './test_data/'
ANCDirPath = '../0_ancillary/'
EMSDirPath = paste0(ANCDirPath,'UW_baseline_fit_emis/update2021/')
OutputDir_LST = './output_examples/'

### SZAC coefficient derived from MODIS best-quality pixels during 2016-2020
szac_coeff_rst = raster(paste0(ANCDirPath, 'SZAC_v1.4.1_log_2km_1to99percentile.tif'))

### australia raster template
aus_template = raster(matrix(NA, nrow=1750, ncol=2100), xmn=112, xmx=154, ymn=-45, ymx=-10, crs=PROJ_LATLON)

if (!dir.exists(OutputDir_LST)) dir.create(OutputDir_LST, recursive=TRUE)

print('')
print(' - - - -  Reading in emissivity information')
e11um_file = paste0(EMSDirPath,'11um/global_emis_11um_monthly_2003-2016_',mm,'.tif')
e12um_file = paste0(EMSDirPath,'12um/global_emis_12um_monthly_2003-2016_',mm,'.tif')

e11 = as.matrix(raster(e11um_file))    # 10.8 um
e12 = as.matrix(raster(e12um_file))    # 12.1 um
e11[is.na(e11)] = 1. 
e12[is.na(e12)] = 1.

eee = 0.5 * (e11 + e12)
rm(e11,e12)

### Ancillary data 
print('')
print( ' - - - -  Processing sensor view angle')
 	
x00 = nc_open(paste0(ANCDirPath,'/20150127000000-P1S-ABOM_GEOM_SENSOR-PRJ_GEOS141_2000-HIMAWARI8-AHI.nc'))
	x = ncvar_get(x00,'sensor_zenith_angle')
	nc_close(x00)
	x[is.na(x)] = -9999
	senZA = t(x); senZA[senZA <0] = NA
rm(x)

### AHI Brightness temperature
print('')
print( ' - - - -  Processing brightness temperature processing')

### create the list containing hours and mins
### here we only use 00:00 and 00:10 as examples
TBfolders = seq(as.POSIXct('2000-01-01 00:00'), as.POSIXct('2000-01-01 00:10'), by=600)
TBfolders = format(TBfolders, '%H%M')

### refine folder list to only those containing data (eliminates 0240 1440, at least)
DirN = length(TBfolders)
for (i in 1:DirN) {

	print( '         .......  looping through the folders and extracting TBs')

	if (DOI < as.Date('2022-10-01')) {

		# Use Himawari-8 data (dual-channel Tb, SZA, cloud mask) before 01/Oct/2022

		flB14 = paste0(TB_DirPath,yyyy,mm,dd,TBfolders[i],'00-P1S-ABOM_OBS_B14-PRJ_GEOS141_2000-HIMAWARI8-AHI.nc')
		flB15 = paste0(TB_DirPath,yyyy,mm,dd,TBfolders[i],'00-P1S-ABOM_OBS_B15-PRJ_GEOS141_2000-HIMAWARI8-AHI.nc')

		flSOL = paste0(SOL_DirPath,yyyy,mm,dd,TBfolders[i],'00-P1S-ABOM_GEOM_SOLAR-PRJ_GEOS141_2000-HIMAWARI8-AHI.nc')

		flCMA = paste0(CMA_DirPath,'S_NWC_CMA_HIMA08_HIMA-N-NR_',yyyy,mm,dd,'T',TBfolders[i],'00Z.nc')

	} else {

		# Use Himawari-9 data (dual-channel Tb, SZA, cloud mask) after 01/Oct/2022

		flB14 = paste0(TB_DirPath,yyyy,mm,dd,TBfolders[i],'00-P1S-ABOM_OBS_B14-PRJ_GEOS141_2000-HIMAWARI9-AHI.nc')
		flB15 = paste0(TB_DirPath,yyyy,mm,dd,TBfolders[i],'00-P1S-ABOM_OBS_B15-PRJ_GEOS141_2000-HIMAWARI9-AHI.nc')

		flSOL = paste0(SOL_DirPath,yyyy,mm,dd,TBfolders[i],'00-P1S-ABOM_GEOM_SOLAR-PRJ_GEOS141_2000-HIMAWARI9-AHI.nc')

		flCMA = paste0(CMA_DirPath,'S_NWC_CMA_HIMA09_HIMA-N-NR_',yyyy,mm,dd,'T',TBfolders[i],'00Z.nc')
	}

	# specify the output file
	lstFile = paste0(OutputDir_LST,yyyy,mm,dd,TBfolders[i],'00_AHI_ANU_LSTv1.4.1_AusSubset.tif')

	if (!file.exists(flB14) | !file.exists(flB15)  | !file.exists(flSOL)) {

		print(paste0('Cannot generate LST for ', yyyy, '-', mm, '-', dd, '-', TBfolders[i]))
		
		# write a NA template if we cannot retrieve the data
		writeRaster(aus_template,lstFile,overwrite=TRUE)

	} else {

		## ---- TB 10.8 micron	
		x00 = nc_open(flB14)
			x = ncvar_get(x00, 'channel_0014_brightness_temperature')
		  	nc_close(x00)
		  	x[is.na(x)] = NA
		  	TB_11um = t(x)

		## ---- TB 12.1 micron	
		x00 = nc_open(flB15)
      	  	x = ncvar_get(x00, 'channel_0015_brightness_temperature')
    	  	nc_close(x00)
    	  	x[is.na(x)] = NA
		  	TB_12um = t(x)

		## ---- Sol Zenith 
        x00 = nc_open(flSOL)
          	x = ncvar_get(x00, 'solar_zenith_angle')
          	nc_close(x00)
          	x[is.na(x)] = NA
		  	solZA = t(x)

        print(paste('                                  +++ completed extraction for',TBfolders[i],'hrs.'))
        print('')
        print( ' - - - -  LST retrieval commencing now . . . ')

		# - - - LST array
		LSTt  = matrix(NA, NROWS,NCOLS)

		# - - - identify DAY and NIGHT cells
		DAY     <- solZA <= 85 & !is.na(solZA)
		NIGHT   <- solZA >  85 & !is.na(solZA)  
		  
		# do LST retrieval
		# use the GOES-R LST algorithm coefficients applied on the GOES-8 Imager
		# reference:
		# Yu, Y., Tarpley, D., Privette, J.L., Flynn, L.E., Xu, H., Chen, M., Vinnikov, K.Y., Sun, D. and Tian, Y., 2011. 
		# Validation of GOES-R satellite land surface temperature algorithm using SURFRAD ground measurements and statistical estimates of error properties. 
		# IEEE Transactions on Geoscience and Remote Sensing, 50(3), pp.704-713.

		LSTt        = 35.022546 + 1.018212 * TB_11um + -39.387858 * eee + 
            		  (1.263787 + 0.609744 * (1./cos(pi*senZA/1.8e+2) - 1.)) * (TB_11um - TB_12um)
		  
        LSTt[NIGHT] = 36.160667 + 1.012895 * TB_11um[NIGHT] + -38.909505 * eee[NIGHT] +
                      (1.022203 + 0.669541 * (1./cos(pi*senZA[NIGHT]/1.8e+2) - 1.)) *
                      (TB_11um[NIGHT] - TB_12um[NIGHT])

		# set the LST range
        LSTmax = 350; LSTmin = 250
        LSTt[is.na(TB_11um)] = NA

		# convert LST matrix to raster and remove outliers
		LST = raster(LSTt,crs=PROJ_GEO, xmn=Emn, xmx=Emx, ymn=Nmn, ymx=Nmx)
		LST[LST>LSTmax] = LSTmax
		LST[LST<LSTmin] = LSTmin

		# project LST to the Australian domain
		aus_subset = projectRaster(LST, aus_template, method='ngb')
		  
		# filter the daytime area 
		rst_DAY = raster(DAY,crs=PROJ_GEO, xmn=Emn, xmx=Emx, ymn=Nmn, ymx=Nmx)
		rst_DAY = projectRaster(rst_DAY, aus_template, method='ngb')
		  
		# read solar zenith angles
		rst_solZA = raster(solZA,crs=PROJ_GEO, xmn=Emn, xmx=Emx, ymn=Nmn, ymx=Nmx)
		rst_solZA = projectRaster(rst_solZA, aus_template, method='ngb')

		# calculate MODIS-derived SZAC and apply to daytime LST
		SZAC = szac_coeff_rst * log(cos(rst_solZA * pi/180) + 1)
		aus_subset[rst_DAY==1] = aus_subset[rst_DAY==1] - SZAC[rst_DAY==1]
		
		# use SZAC to mask out oceans
		aus_subset = mask(aus_subset, SZAC)

		# read the BoM cloud mask
		cma = raster(flCMA, varname='cma')
        cma = projectRaster(cma, aus_template, method='ngb')

        # apply cloud mask; 1 = cloud, 0 = clear
        aus_subset[cma[] == 1] = NA

		writeRaster(aus_subset,lstFile,overwrite=TRUE)
		rm(x,LST,LSTt,TB_11um,TB_12um)
	}
}
