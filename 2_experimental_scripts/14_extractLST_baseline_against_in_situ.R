# ################################################################
# objective:   extract in-situ and multi lst data 
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_SetupForGraphics.R')
library(foreach)
library(doParallel)
library(lubridate)

cl = makeCluster(22)
registerDoParallel(cl)

# start to do the extraction of lst data

path2anu = '/g/data/dt1/SatelliteLST/H8_LST_ANU_baseline/'
path2chiba = '/g/data/dt1/SatelliteLST/H8_LST_Chiba/'
output = '/g/data/os22/users/yu/himawari/evaluations/anu_baseline_for_fitting_updated/'

if (!dir.exists(output)) dir.create(output)

ozfluxLL = read.csv('/g/data/os22/users/yu/himawari/0_code/emis_lookup.csv')

# time range for evaluation
TOIs =  seq(ISOdatetime(2016,1,1,1,0,0,tz='GMT'),ISOdatetime(2020,12,31,23,0,0,tz='GMT'),3600)

foreach (i=1:nrow(ozfluxLL), .combine=cbind, .packages=c('lubridate', 'raster')) %dopar% {

    getCellfromLocation = function(Lat,Long,Raster) {
        # Raster must be a raster object
        projinfo = as.character(crs(Raster))
        ProjLatLon = '+proj=longlat +datum=WGS84'
                ll = SpatialPoints(cbind(Long,Lat),proj4string=CRS(ProjLatLon))
                en = spTransform(ll,CRS(projinfo))
        return(cellFromXY(Raster,en))
    }
    
    print(paste0('start extracting value for site ',ozfluxLL$sitename[i]))

    longwaveRad = read.csv(paste0('/g/data/fj4/himawari/OzFluxData/0_LongwaveRadiation_UseThis/', ozfluxLL$sitename[i], '_longwaveRad.csv'))

    ofile = paste0(output, ozfluxLL$sitename[i], '_extracted_data.csv')
    basic = data.frame(matrix(nrow=0,ncol=5))
    colnames(basic) = c('local_time','flux','anu','chiba','solar_angle')
    write.table(basic, ofile, row.names=FALSE, sep=',', quote=FALSE)

    # read the ground obs of radiation
    L_u = longwaveRad$LW_u; L_d = longwaveRad$LW_d

    TZ = ozfluxLL$standard_timezone[i]
    T = ISOdatetime(1800,1,1,0,0,0,tz=TZ) + seconds(longwaveRad$time_x*3600*24)

    for (k in 1:length(TOIs)) {	

        # emis value using lookup table
        month_colval = as.numeric(format(TOIs[k], '%m')) + 5
        emis_Ts = ozfluxLL[i, month_colval]

        # anu lst data
        lstdate = format(TOIs[k],'%Y/%m/%d/')
        lstfile = paste0(path2anu,lstdate,format(TOIs[k],'%Y%m%d%H%M'), '00_AHI_ANU_LSTv1.0_AusSubset.tif')
        
        if (!file.exists(lstfile)) {
            print(paste(' ---- No file for ANU LST at ',TOIs[k]))
            LST_anu = NA
        } else {
            lst = raster(lstfile)
            LST_anu = lst[getCellfromLocation(ozfluxLL$lat[i],ozfluxLL$lon[i],lst)]
            print(LST_anu)
        }

        # chiba lst data
        lstdate = format(TOIs[k],'%Y/%m/%d/')
        lstfile = paste0(path2chiba,lstdate,format(TOIs[k],'%Y%m%d%H%M'), '_AHI_Chiba_LSTv0_AusSubset.tif')
        
        if (!file.exists(lstfile)) {
            print(paste(' ---- No file for Chiba LST at ',TOIs[k]))
            LST_chiba = NA
        } else {
            lst = raster(lstfile)
            LST_chiba = lst[getCellfromLocation(ozfluxLL$lat[i],ozfluxLL$lon[i],lst)]
            print(LST_chiba)
        }

        # flux tower Ts
        timeInterval = c(TOIs[k]-1800,TOIs[k]+1800)
        attr(timeInterval,'tzone') = TZ
        whichT = which(T > timeInterval[1] & T < timeInterval[2])
        rad_u_TOI = mean(L_u[whichT],na.rm=TRUE)
        rad_d_TOI = mean(L_d[whichT],na.rm=TRUE)

        flux_ts = ((rad_u_TOI - (1 - emis_Ts) * rad_d_TOI) / (5.670374e-8 * emis_Ts)) ^ (1/4)

        # solar angle
        flSOLDir = paste0('/g/data/ra22/satellite-products/arc/obs/himawari-ahi/fldk/latest/',
                          format(TOIs[k], '%Y/%m/%d/%H%M/'))
        flSOL    = paste0(flSOLDir,format(TOIs[k], '%Y%m%d%H%M'),'00-P1S-ABOM_GEOM_SOLAR-PRJ_GEOS141_2000-HIMAWARI8-AHI.nc')
		
        if (!file.exists(flSOL)) {
            print(paste(' ---- No file for solar zenith angle at ',TOIs[k]))
		    sol_ang = NA
        } else {
            rst = raster(flSOL, varname = 'solar_zenith_angle')
            sol_ang = rst[getCellfromLocation(ozfluxLL$lat[i],ozfluxLL$lon[i],rst)]
        }

        local_time = TOIs[k]; attr(local_time, 'tzone') = TZ

        write.table(cbind(format(local_time, '%Y-%m-%d %H:%M'),flux_ts, LST_anu, LST_chiba, sol_ang),
                    ofile,quote=FALSE,row.names=FALSE,col.names=FALSE,sep=',',append=TRUE)
		
    }
}
