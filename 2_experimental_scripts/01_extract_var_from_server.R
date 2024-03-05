# ################################################################
# objective:   extract longwave radiation from OzFlux server
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

library(ncdf4)
library(stringr)

sitelist = read.csv('/g/data/os22/users/yu/himawari/0_code/OzFluxSites_LatLon.csv')

for (k in 1:nrow(sitelist)){
    sitename = sitelist$sitename[k]
    print(sitename)

    x = nc_open(paste0('https://dap.ozflux.org.au/thredds/dodsC/ozflux/sites/',sitename,'/L3/default/',sitename,'_L3.nc'))
    Fld = ncvar_get(x, 'Fld')
    Flu = ncvar_get(x, 'Flu')
    local_time = ncvar_get(x, 'time')

    df_LW = data.frame(time_x = local_time, LW_u = Flu, LW_d = Fld)

    write.table(df_LW, paste0('/g/data/fj4/himawari/OzFluxData/LongwaveRadiation/',sitename,'_longwaveRad.csv'), 
                row.names=FALSE, quote=FALSE, sep=',')

}

