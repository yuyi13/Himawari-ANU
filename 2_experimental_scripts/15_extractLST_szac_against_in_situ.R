# ################################################################
# objective:   extract anu_szac lst (v1.4.1) data
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_SetupForGraphics.R')
library(foreach)
library(doParallel)

cl = makeCluster(22)
registerDoParallel(cl)

# start to do the extraction of lst data

path2anu = '/g/data/lr26/Himawari-AHI_LST_ANU/'
path2chiba = '/g/data/dt1/SatelliteLST/H8_LST_Chiba/'
output = '/g/data/os22/users/yu/himawari/evaluations/anu_v1.4.1/'

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
    basic = data.frame(matrix(nrow=0,ncol=2))
    colnames(basic) = c('local_time','anu_v1.4.1')
    write.table(basic, ofile, row.names=FALSE, sep=',', quote=FALSE)

    # read the ground obs of radiation
    L_u = longwaveRad$LW_u; L_d = longwaveRad$LW_d

    TZ = ozfluxLL$timezone[i]
    T = ISOdatetime(1800,1,1,0,0,0,tz=TZ) + longwaveRad$time_x*3600*24

    for (k in 1:length(TOIs)) {	

        # emis value using lookup table
        month_colval = as.numeric(format(TOIs[k], '%m')) + 4
        emis_Ts = ozfluxLL[i, month_colval]

        # anu lst data
        lstdate = format(TOIs[k],'%Y/%m/%d/')
        lstfile = paste0(path2anu,lstdate,format(TOIs[k],'%Y%m%d%H%M'), '00_AHI_ANU_LSTv1.4.1_AusSubset.tif')
        
        if (!file.exists(lstfile)) {
            print(paste(' ---- No file for ANU LST at ',TOIs[k]))
            LST_anu = NA
        } else {
            lst = raster(lstfile)
            LST_anu = lst[getCellfromLocation(ozfluxLL$lat[i],ozfluxLL$lon[i],lst)]
            print(LST_anu)
        }

        local_time = TOIs[k]; attr(local_time, 'tzone') = TZ

        write.table(cbind(format(local_time, '%Y-%m-%d %H:%M'),LST_anu),
                    ofile,quote=FALSE,row.names=FALSE,col.names=FALSE,sep=',',append=TRUE)
		
    }
}
