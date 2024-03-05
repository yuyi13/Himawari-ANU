# ################################################################
# objective:   create a look-up table for emissivity
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_SetupForGraphics.R')

ozfluxLL = read.csv('/g/data/os22/users/yu/himawari/0_code/OzFluxSites_LatLon.csv')

# create an emis look-up table
ANCDirPath = '/g/data/fj4/himawari/ANCILLARY/'
EMSDirPath =  paste0(ANCDirPath,'EMISS_SSEC/update2021/')

emis_file = paste0('/g/data/os22/users/yu/himawari/0_code/algorithm_test/emis_lookup.csv')
emis_month = seq(as.Date('2000-01-01'), as.Date('2000-12-01'), by='month')

for (m in 1:length(emis_month)){
    
    mm = format(emis_month[m], '%m')
    e11um_file = paste0(EMSDirPath,'11um/global_emis_11um_monthly_2003-2016_',mm,'.tif')
    e12um_file = paste0(EMSDirPath,'12um/global_emis_12um_monthly_2003-2016_',mm,'.tif')

    e11 = raster(e11um_file)   # 10.8 micron
    e12 = raster(e12um_file)   # 12.1 micron
    e11[is.na(e11)] = 1. 
    e12[is.na(e12)] = 1.

    eee = 0.5 * (e11 + e12)
    rm(e11,e12)

    monthly_val = c()
    for (i in 1:nrow(ozfluxLL)){

        emis_aoi = eee[getCellfromLocation(ozfluxLL$lat[i],ozfluxLL$lon[i],eee)]
        monthly_val = c(monthly_val, emis_aoi)
    }

    print(paste0('Finish for month ', mm))
    ozfluxLL[, 4+m] = monthly_val
}

colnames(ozfluxLL) = c(colnames(ozfluxLL)[1:4], format(emis_month, '%m'))
write.table(ozfluxLL, emis_file, sep=',', row.names=FALSE, quote=FALSE)
