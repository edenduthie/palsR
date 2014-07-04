# 2DAusMean.R
#
# Gab Abramowitz, UNSW, 2014, gabsun at gmail dot com
#
SpatialAusMean = function(model,obs=NULL,bench=NULL,varname,unitstxt,longvarname,metrics){
	library(maps)
	library(mapdata)
	library(fields) # for image.plot
	library(colorRamps)
	errtext = 'ok'
	allcols = colorRampPalette(c('red','orange','yellow','green','blue'))
	# calculate number of map panels:
	npanels = bench$howmany + 3
	if(is.null(obs) && is.null(bench)){
		image.plot(obs$lon,obs$lat,TimeMean(model$data),col=allcols,xlab='Longitude',ylab='Latitude')
		map('worldHires',add=TRUE,wrap=TRUE,xlim=c(min(obs$lon),max(obs$lon)),ylim=c(min(obs$lat),max(obs$lat)))
		title(paste(model$name,longvarname))
	}else if(npanels<=4){	
		par(mfcol=c(2,2),mar=c(4,4,3,0.5),oma=c(0,0,0,1),mgp=c(2.5,0.7,0),ps=12,tcl=-0.4)
		# First plot: model
		modelmean = TimeMean(model$data)
		obsmean = TimeMean(obs$data) + modelmean - modelmean # to make sure ocean areas not included
		benchmean1 = TimeMean(bench[[1]]$data)
		zrange = c(min(modelmean,obsmean,na.rm=TRUE),max(modelmean,obsmean,na.rm=TRUE))
		diffrange = c(min(modelmean-obsmean,benchmean1-obsmean,na.rm=TRUE),
			max(modelmean-obsmean,benchmean1-obsmean,na.rm=TRUE))
		image.plot(obs$lon,obs$lat,modelmean,xlab='Longitude',ylab='Latitude',zlim=zrange)
		map('worldHires',add=TRUE,wrap=TRUE,xlim=c(min(obs$lon),max(obs$lon)),ylim=c(min(obs$lat),max(obs$lat)))
		title(paste(model$name,longvarname))
		# Second plot: obs 
		image.plot(obs$lon,obs$lat,obsmean,xlab='Longitude',ylab='Latitude',zlim=zrange)
		map('worldHires',add=TRUE,wrap=TRUE,xlim=c(min(obs$lon),max(obs$lon)),ylim=c(min(obs$lat),max(obs$lat)))
		title(paste(obs$name,longvarname))
		# Third plot: difference model, obs 
		image.plot(obs$lon,obs$lat,(modelmean-obsmean),xlab='Longitude',ylab='Latitude',zlim=diffrange)
		map('worldHires',add=TRUE,wrap=TRUE,xlim=c(min(obs$lon),max(obs$lon)),ylim=c(min(obs$lat),max(obs$lat)))
		title(paste(model$name,'-',obs$name,longvarname))
		# Fourth plot: difference bench, obs 
		image.plot(obs$lon,obs$lat,(benchmean1 - obsmean),xlab='Longitude',ylab='Latitude',zlim=diffrange)
		map('worldHires',add=TRUE,wrap=TRUE,xlim=c(min(obs$lon),max(obs$lon)),ylim=c(min(obs$lat),max(obs$lat)))
		title(paste(bench[[1]]$name,'-',obs$name,longvarname))
	}else{	
		stop('blah')
	}
	result = list(errtext=errtext)
	return(result)
}

TimeMean = function(threedvar){
	twodvar = aperm(apply(threedvar,1,rowSums)) / length(threedvar[1,1,])	
	return(twodvar)
}