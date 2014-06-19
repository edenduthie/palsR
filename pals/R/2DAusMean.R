# 2DAusMean.R
#
# Gab Abramowitz, UNSW, 2014, gabsun at gmail dot com
#
SpatialAusMean = function(lon,lat,moutput,obs=NULL,bench1=NULL,bench2=NULL,bench3=NULL){
	#library(maps)
	#library(mapdata)
	
	# calculate number of map panels:
	npanels = 1 + as.numeric((! is.null(obs))) + as.numeric((! is.null(bench1))) +
		as.numeric((! is.null(bench2))) + as.numeric((! is.null(bench3)))
	
	image.plot(lon,lat,meanplot,col=allcols,
	xlab='Longitude',ylab='Latitude',zlim=zrange)
	map('worldHires',add=TRUE,wrap=TRUE,xlim=c(-180,180),ylim=c(-90,90))
	title(paste('(a) dT 2080-99 vs 1980-99 - CMIP5 mean',rcp))

}