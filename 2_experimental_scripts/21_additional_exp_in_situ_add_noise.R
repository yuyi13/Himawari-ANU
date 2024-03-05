# ################################################################
# objective:   add random nosie and assess the uncertainty 
#              of the in-situ data  
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

# reference: Martin, M. A., Ghent, D., Pires, A. C., Göttsche, F.-M., Cermak, J. and Remedios, J. J., 2019. 
# Comprehensive In Situ Validation of Five Satellite Land Surface Temperature Data Sets over Multiple Stations and Years. Remote Sens., 11, 479. 
# https://doi.org/10.3390/rs11050479
# Section 2.2.2. SURFRAD Stations

source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_SetupForGraphics.R')
library(foreach)
library(doParallel)
library(lubridate)

cl = makeCluster(22)
registerDoParallel(cl)

# start to do the extraction of lst data

output = '/datasets/work/d61-af-soilmoisture/work/himawari/in_situ_evaluations/in_situ_lst_with_random_noise/'

if (!dir.exists(output)) dir.create(output)

ozfluxLL = read.csv('/datasets/work/d61-af-soilmoisture/work/himawari/in_situ_evaluations/emis_lookup.csv')

# time range for evaluation
TOIs =  seq(ISOdatetime(2016,1,1,1,0,0,tz='GMT'),ISOdatetime(2020,12,31,23,0,0,tz='GMT'),3600)

foreach (i=1:nrow(ozfluxLL), .combine=cbind, .packages=c('lubridate')) %dopar% {

    source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_SetupForGraphics.R')

    print(paste0('start extracting value for site ',ozfluxLL$sitename[i]))

    longwaveRad = read.csv(paste0('/datasets/work/d61-af-soilmoisture/work/himawari/in_situ_evaluations/0_LongwaveRadiation_UseThis/', ozfluxLL$sitename[i], '_longwaveRad.csv'))

	ofile = paste0(output, ozfluxLL$sitename[i], '_lst_with_noise.csv')
    basic = data.frame(matrix(nrow=0,ncol=2))
	colnames(basic) = c('local_time','flux')
    write.table(basic, ofile, row.names=FALSE, sep=',', quote=FALSE)

    # read the ground obs of radiation and add random noise
	L_u = longwaveRad$LW_u
    set.seed(13); L_u_with_noise = L_u + runif(length(L_u), -5, 5)
    
    L_d = longwaveRad$LW_d
    set.seed(13); L_d_with_noise = L_d + runif(length(L_d), -5, 5)

    TZ = ozfluxLL$standard_timezone[i]
    T = ISOdatetime(1800,1,1,0,0,0,tz=TZ) + seconds(longwaveRad$time_x*3600*24)

    # add random noise to the monthly emis data
    set.seed(13)
    emis_with_noise = ozfluxLL[i, 6:17] + rnorm(12, 0, 0.01)

    for (k in 1:length(TOIs)) {	

        # emis value using lookup table
        month_colval = as.numeric(format(TOIs[k], '%m'))
        emis_Ts = emis_with_noise[month_colval]

        # flux tower Ts
        timeInterval = c(TOIs[k]-1800,TOIs[k]+1800)
		attr(timeInterval,'tzone') = TZ
        whichT = which(T > timeInterval[1] & T < timeInterval[2])
		rad_u_TOI = mean(L_u_with_noise[whichT],na.rm=TRUE)
        rad_d_TOI = mean(L_d_with_noise[whichT],na.rm=TRUE)

        flux_ts_with_noise = ((rad_u_TOI - (1 - emis_Ts) * rad_d_TOI) / (5.670374e-8 * emis_Ts)) ^ (1/4)

        local_time = TOIs[k]; attr(local_time, 'tzone') = TZ

		write.table(cbind(format(local_time, '%Y-%m-%d %H:%M'),flux_ts_with_noise),
                    ofile,quote=FALSE,row.names=FALSE,col.names=FALSE,sep=',',append=TRUE)
		
    }
}
