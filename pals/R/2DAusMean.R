# 2DAusMean.R
#
# Gab Abramowitz, UNSW, 2014, gabsun at gmail dot com
#
SpatialAusMeanDiff = function(model,obs=NULL,bench=NULL,varname,unitstxt,longvarname,metrics){
	errtext = 'ok'
	density_cut = 1/200
	# Calculate number of map panels:
	npanels = bench$howmany + 3
	# Plot layout:
	if(npanels <= 4){
		par(mfcol=c(2,2) ,mar=c(4,4,3,0.5),oma=c(0,0,0,1),mgp=c(2.5,0.7,0),ps=14,tcl=-0.4)
		density_location = list()
		density_location[[1]] = c(0.06,0.3,0.6,0.69)
		density_location[[2]] = c(0.56,0.8,0.6,0.69)
		density_location[[3]] = c(0.06,0.3,0.1,0.19)
		density_location[[4]] = c(0.56,0.8,0.1,0.19)
		textloc='bottomright'
	}else{
		par(mfcol=c(2,3) ,mar=c(4,4,3,0.5),oma=c(0,0,0,1),mgp=c(2.5,0.7,0),ps=18,tcl=-0.4)
		density_location = list()
		density_location[[1]] = c(0.04,0.2,0.59,0.68)
		density_location[[2]] = c(0.37,0.53,0.59,0.68)
		density_location[[3]] = c(0.72,0.87,0.59,0.68)
		density_location[[4]] = c(0.04,0.2,0.09,0.18)
		density_location[[5]] = c(0.37,0.53,0.09,0.18)
		density_location[[6]] = c(0.72,0.87,0.09,0.18)
		textloc='topleft'
	}
	# Calculate time means:
	modelmean = TimeMean(model$data)
	obsmean = TimeMean(obs$data) + modelmean - modelmean # to make sure ocean areas not included
	zrange = c(min(modelmean,obsmean,na.rm=TRUE),max(modelmean,obsmean,na.rm=TRUE))
	diffrange = c(min(modelmean-obsmean,na.rm=TRUE),max(modelmean-obsmean,na.rm=TRUE))
	if(bench$howmany == 1){
		# Get benchmark data, noting there may have been other benchmarks that failed (so use bench$index)
		benchmean1 = TimeMean(bench[[ bench$index[1] ]]$data)
		diffrange = c(min(modelmean-obsmean,benchmean1-obsmean,na.rm=TRUE),
			max(modelmean-obsmean,benchmean1-obsmean,na.rm=TRUE))
	}else if(bench$howmany == 2){
		# Get benchmark data, noting there may have been other benchmarks that failed (so use bench$index)
		benchmean1 = TimeMean(bench[[ bench$index[1] ]]$data)
		benchmean2 = TimeMean(bench[[ bench$index[2] ]]$data)
		diffrange = c(min(modelmean-obsmean,benchmean1-obsmean,benchmean2-obsmean,na.rm=TRUE),
			max(modelmean-obsmean,benchmean1-obsmean,benchmean2-obsmean,na.rm=TRUE))
	}else if(bench$howmany == 3){
		benchmean1 = TimeMean(bench[[ bench$index[1] ]]$data)
		benchmean2 = TimeMean(bench[[ bench$index[2] ]]$data)
		benchmean3 = TimeMean(bench[[ bench$index[3] ]]$data)
		diffrange = c(min(modelmean-obsmean,benchmean1-obsmean,benchmean2-obsmean,benchmean3-obsmean,na.rm=TRUE),
			max(modelmean-obsmean,benchmean1-obsmean,benchmean2-obsmean,benchmean3-obsmean,na.rm=TRUE))
	}
	# Fetch colour scheme:
	zcols = ChooseColours(zrange,varname,'positive')
	diffcols = ChooseColours(diffrange,varname,'difference')
	
	# First plot: model	mean
	title = paste(model$name,' ',longvarname,sep='')
	errtext = SpatialAus(obs$lon,obs$lat,modelmean,mean(modelmean,na.rm=T),sd(modelmean,na.rm=T),
		varname,unitstxt,longvarname,zrange,zcols,title,textloc)
	# Second plot: obs mean
	title = paste(obs$name,' ',longvarname,sep='')
	errtext = SpatialAus(obs$lon,obs$lat,obsmean,mean(obsmean,na.rm=T),sd(obsmean,na.rm=T),
		varname,unitstxt,longvarname,zrange,zcols,title,textloc)
	# Third plot: difference of model, obs mean
	title = paste('[',model$name,'-',obs$name,'] ',longvarname,sep='')
	errtext = SpatialAus(obs$lon,obs$lat,(modelmean-obsmean),mean((modelmean-obsmean),na.rm=T),
		sd((modelmean-obsmean),na.rm=T),varname,unitstxt,longvarname,diffrange,diffcols,title,textloc)
	# Plot benchmarks that exist:
	if(bench$exist){
		# Fourth plot: difference bench, obs 
		title = paste('[',bench[[ bench$index[1] ]]$name,'-',obs$name,'] ',longvarname,sep='')
		errtext = SpatialAus(obs$lon,obs$lat,(benchmean1 - obsmean),mean((benchmean1-obsmean),na.rm=T),
			sd((benchmean1-obsmean),na.rm=T),varname,unitstxt,longvarname,diffrange,diffcols,title,textloc)
		if(bench$howmany >= 2){
			title = paste('[',bench[[ bench$index[2] ]]$name,'-',obs$name,'] ',longvarname,sep='')
			errtext = SpatialAus(obs$lon,obs$lat,(benchmean2 - obsmean),mean((benchmean2-obsmean),na.rm=T),
				sd((benchmean2-obsmean),na.rm=T),varname,unitstxt,longvarname,diffrange,diffcols,title,textloc)
			if(bench$howmany == 3){	
				title = paste('[',bench[[ bench$index[3] ]]$name,'-',obs$name,'] ',longvarname,sep='')
				errtext = SpatialAus(obs$lon,obs$lat,(benchmean3 - obsmean),mean((benchmean3-obsmean),na.rm=T),
					sd((benchmean3-obsmean),na.rm=T),varname,unitstxt,longvarname,diffrange,diffcols,title,textloc)
			}
		}	
	}
	### Add density insets ###
	mod_den = density(modelmean,na.rm=TRUE) # calculate model density estimate 
	obs_den = density(obsmean,na.rm=TRUE) # calculate obs density estimate
	ymax = max(mod_den[[2]],obs_den[[2]]) # max value for these two plots
	# Determine where to truncate x-axis according to density cut threshold:
	xmin = min(mod_den[[1]][ (mod_den[[2]]>(ymax*density_cut)) ],obs_den[[1]][ (obs_den[[2]]>(ymax*density_cut)) ])
	xmax = max(mod_den[[1]][ (mod_den[[2]]>(ymax*density_cut)) ],obs_den[[1]][ (obs_den[[2]]>(ymax*density_cut)) ])
	# Plot pdfs for model and obs
	in1 = InsetDensity(density_location[[1]],mod_den,c(xmin,xmax))
	in2 = InsetDensity(density_location[[2]],obs_den,c(xmin,xmax))
	moderr_den = density((modelmean-obsmean),na.rm=TRUE) # calculate model error density estimate 
	ymax = max(moderr_den[[2]]) # max value
	# Determine where to truncate x-axis according to density cut threshold:
	xmin = min(moderr_den[[1]][ (moderr_den[[2]]>(ymax*density_cut)) ])
	xmax = max(moderr_den[[1]][ (moderr_den[[2]]>(ymax*density_cut)) ])
	if(bench$howmany == 1){
		bencherr_den = density((benchmean1-obsmean),na.rm=TRUE)
		# If there's a benchmark, overwrite plot x and y limits to include benchmark info:
		ymax = max(moderr_den[[2]],bencherr_den[[2]])
		xmin = min(moderr_den[[1]][ (moderr_den[[2]]>(ymax*density_cut)) ],bencherr_den[[1]][ (bencherr_den[[2]]>(ymax*density_cut)) ])
		xmax = max(moderr_den[[1]][ (moderr_den[[2]]>(ymax*density_cut)) ],bencherr_den[[1]][ (bencherr_den[[2]]>(ymax*density_cut)) ])
		in4 = InsetDensity(density_location[[4]],bencherr_den,c(xmin,xmax))
	}else if(bench$howmany == 2){
		bench1err_den = density((benchmean1-obsmean),na.rm=TRUE)
		bench2err_den = density((benchmean2-obsmean),na.rm=TRUE)
		ymax = max(moderr_den[[2]],bench1err_den[[2]],bench2err_den[[2]])
		xmin = min(moderr_den[[1]][ (moderr_den[[2]]>(ymax*density_cut)) ],
			bench1err_den[[1]][ (bench1err_den[[2]]>(ymax*density_cut)) ],
			bench2err_den[[1]][ (bench2err_den[[2]]>(ymax*density_cut)) ] )
		xmax = max(moderr_den[[1]][ (moderr_den[[2]]>(ymax*density_cut)) ],
			bench1err_den[[1]][ (bench1err_den[[2]]>(ymax*density_cut)) ],
			bench2err_den[[1]][ (bench2err_den[[2]]>(ymax*density_cut)) ] )
		in4 = InsetDensity(density_location[[4]],bench1err_den,c(xmin,xmax))	
		in5 = InsetDensity(density_location[[5]],bench2err_den,c(xmin,xmax))	
	}else if(bench$howmany == 3){
		bench1err_den = density((benchmean1-obsmean),na.rm=TRUE)
		bench2err_den = density((benchmean2-obsmean),na.rm=TRUE)
		bench3err_den = density((benchmean3-obsmean),na.rm=TRUE)
		ymax = max(moderr_den[[2]],bench1err_den[[2]],bench2err_den[[2]],bench3err_den[[2]])
		xmin = min(moderr_den[[1]][ (moderr_den[[2]]>(ymax*density_cut)) ],
			bench1err_den[[1]][ (bench1err_den[[2]]>(ymax*density_cut)) ],
			bench2err_den[[1]][ (bench2err_den[[2]]>(ymax*density_cut)) ],
			bench3err_den[[1]][ (bench3err_den[[2]]>(ymax*density_cut)) ])
		xmax = max(moderr_den[[1]][ (moderr_den[[2]]>(ymax*density_cut)) ],
			bench1err_den[[1]][ (bench1err_den[[2]]>(ymax*density_cut)) ],
			bench2err_den[[1]][ (bench2err_den[[2]]>(ymax*density_cut)) ],
			bench3err_den[[1]][ (bench3err_den[[2]]>(ymax*density_cut)) ])
		in4 = InsetDensity(density_location[[4]],bench1err_den,c(xmin,xmax))	
		in5 = InsetDensity(density_location[[5]],bench2err_den,c(xmin,xmax))
		in6 = InsetDensity(density_location[[6]],bench3err_den,c(xmin,xmax))		
	}
	in3 = InsetDensity(density_location[[3]],moderr_den,c(xmin,xmax))
	result = list(errtext=errtext)
	return(result)
}

TimeMean = function(threedvar){
	# Take the time mean of 3D variable
	twodvar = aperm(apply(threedvar,1,rowSums)) / length(threedvar[1,1,])	
	return(twodvar)
}

SpatialAus = function(lon,lat,data,meanval,sdval,varname,unitstxt,longvarname,zrange,zcols,title,textloc='default'){
	# Generates a gridded heatmap style plot for Australia with map, based on input lat/lon.
	library(maps)
	library(mapdata)
	library(fields) # for image.plot
	errtext = 'ok'
	# Decide location of plot for text placement:
	if((textloc=='default') | (textloc=='bottomright')){
		textloc1 = c((obs$lon[1] + (obs$lon[length(obs$lon)] - obs$lon[1])*0.9), (obs$lat[1] + (obs$lat[length(obs$lat)] - obs$lat[1])*0.13) )
		textloc2 = c((obs$lon[1] + (obs$lon[length(obs$lon)] - obs$lon[1])*0.9), (obs$lat[1] + (obs$lat[length(obs$lat)] - obs$lat[1])*0.07) )
	}else if(textloc=='topleft'){
		textloc1 = c((obs$lon[1] + (obs$lon[length(obs$lon)] - obs$lon[1])*0.15), (obs$lat[1] + (obs$lat[length(obs$lat)] - obs$lat[1])*0.93) )
		textloc2 = c((obs$lon[1] + (obs$lon[length(obs$lon)] - obs$lon[1])*0.15), (obs$lat[1] + (obs$lat[length(obs$lat)] - obs$lat[1])*0.87) )
	}
	# Plot:	
	image.plot(lon,lat,data,xlab='Longitude',ylab='Latitude',col=zcols,zlim=zrange)
	map('worldHires',add=TRUE,wrap=TRUE,xlim=c(min(lon),max(lon)),ylim=c(min(lat),max(lat))) # Add map
	title(title) 
	text(x=textloc1[1],y=textloc1[2],labels=paste('Mean:',signif(meanval,3)))
	text(x=textloc2[1],y=textloc2[2],labels=paste('SD:',signif(sdval,2)))
	return(errtext)
}

InsetDensity = function(location,densitydata,xrange){
	# Adds an inset density plot
	par(fig=location,new=T)
	plot(densitydata,lwd=3,main='',ylab='',xlab='',cex.axis=0.8,bty='n',mgp=c(2,0,0),yaxt='n',xlim=xrange,tcl=-0.2)
}
