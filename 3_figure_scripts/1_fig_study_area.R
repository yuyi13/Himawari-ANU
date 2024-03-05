# ################################################################
# objective:   generate plot for the study area
# author:      Yi Yu (yi.yu.phd@gmail.com)
# last update: 2024-03-05
# no runable code here, just for reference
##################################################################

source('~/Workspace/RainfallSpectralAnalysis/SpectralAnalysis/function_SetupForGraphics.R')

x = raster('/datasets/work/d61-af-soilmoisture/work/himawari/0_code/study_area.tif')
site = read.csv('/datasets/work/d61-af-soilmoisture/work/himawari/in_situ_evaluations/emis_lookup.csv')

# exclude Howard Springs and Litchfield
dropped_rows = which(site$sitename == 'HowardSprings' | site$sitename == 'Litchfield')
site = site[-dropped_rows,]

# entire Australia
png('/datasets/work/d61-af-soilmoisture/work/himawari/figures/australia.png', width=800, height=800)

LandCoverRamp =
colorRampPalette(c('blue3','lightgoldenrod','lightgoldenrod3','navajowhite2','lemonchiffon','mediumseagreen','firebrick4'))

image(x, col=LandCoverRamp(7), ext=c(112,154,-45,-10), xlab = NA, ylab = NA, cex.axis=2) 
addCoastLines(Proj=PROJ_LATLON, Colour='black')
scalebar(1000, divs=4, type='bar', cex=2, below='km')
for (k in 1:nrow(site)){
    points(site$lon[k], site$lat[k], pch=17, col='red', cex=3)
}

abline(v=140.7, col='red', lwd=3, lty=2)
dev.off()

# homogeneous area around each site
png('/datasets/work/d61-af-soilmoisture/work/himawari/figures/homogeneity_each_site.png', width=2100, height=900)

LandCoverRamp =
colorRampPalette(c('blue3','lightgoldenrod','lightgoldenrod3','navajowhite2','lemonchiffon','mediumseagreen','firebrick4'))

layout(rbind(c(1,2,3,4,5,6,7),c(8,9,10,11,12,13,14),c(15,16,17,18,19,20,21)))
par(mar = c(0.2, 0.2, 0.2, 0.2))

for (k in 1:nrow(site)){

    sub_region = crop(x, extent(site$lon[k]-0.025, site$lon[k]+0.025, site$lat[k]-0.025, site$lat[k]+0.025))
    
    image(sub_region, zlim=c(101,107), col=LandCoverRamp(7), xlab = NA, ylab = NA, xaxt='n', yaxt='n')

    points(site$lon[k], site$lat[k], pch=17, col='red', cex=6)

    if (site$sitename[k] == 'GreatWesternWoodlands'){
        legend(xmin(sub_region)-0.005, ymax(sub_region), legend=paste0(site$sitename[k]), bty='n', cex=3)
    } else {
        legend(xmin(sub_region)-0.005, ymax(sub_region), legend=paste0(site$sitename[k]), bty='n', cex=3.5)
    }
}

dev.off()

# chiba observation
lst = raster('/datasets/work/d61-af-soilmoisture/work/himawari/0_code/calib/chiba_201601010000.tif') + 273.15

png('/datasets/work/d61-af-soilmoisture/work/himawari/figures/study_area_ap.png', width=800, height=800)

image(lst, col=TemperatureRamp(64), zlim=c(230,330), xlab = NA, ylab = NA, cex.axis=2) 
addCoastLines(Proj=PROJ_LATLON, Colour='black')

rect(112, -45, 154, -10, border='black', lwd=3)

abline(v=140.7, col='red', lwd=3, lty=2)
points(140.7, 0, pch=19, col='red', cex=3)

dev.off()
