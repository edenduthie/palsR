# 2DAus.R
#
# Gab Abramowitz, UNSW, 2015, gabsun at gmail dot com
#
SpatialAus = function(model,obs,bench,md,variable,plottype){
	# Used for plots that show obs panel, model panel, model-obs panel and 
	# up to 3 benchmark-obs panels (i.e. max 6 panels).
	# Density plots are always in the lower left, mean and SD values are either bottom
	# right <= 4 panels, or top left for > 4 panels
	errtext = 'ok'
	varname = variable[['Name']][1]
	unitstxt = variable[['UnitsText']]
	longvarname = variable[['PlotName']]
	metrics = list()
	density_cut = 1/200 # truncate denisty plot x-axis at this fraction of max y value 
	npanels = bench$howmany + 3 # Number of map panels in plot
	
	# Plot layout:
	if(npanels <= 4){
		par(mfcol=c(2,2) ,mar=c(3,3,3,0.5),oma=c(0,0,0,1),mgp=c(1.8,0.5,0),ps=15,tcl=-0.4)
		density_location = DensityLocationAus(4)
		textloc='bottomright'
	}else{
		par(mfcol=c(2,3) ,mar=c(3,3,3,0.5),oma=c(1,0,0.5,1),mgp=c(1.8,0.5,0),ps=18,tcl=-0.2)
		density_location = DensityLocationAus(6)
		textloc='topleft'
	}
	
	# Fetch colour scheme:
	zcols = ChooseColours(md$zrange,varname,'positive')
	diffcols = ChooseColours(md$diffrange,varname,'difference')
	
	### Draw plot panels ###
	# First plot: model	
	title = paste(model$name,' ',longvarname,' ',plottype,sep='')
	errtext = PlotAus(obs$grid$lon,obs$grid$lat,md$modelm,mean(md$modelm,na.rm=T),sd(md$modelm,na.rm=T),
		varname,unitstxt,longvarname,md$zrange,zcols,title,textloc)
	# Second plot: obs 
	title = paste(obs$name,' ',longvarname,' ',plottype,sep='')
	errtext = PlotAus(obs$grid$lon,obs$grid$lat,md$obsm,mean(md$obsm,na.rm=T),sd(md$obsm,na.rm=T),
		varname,unitstxt,longvarname,md$zrange,zcols,title,textloc)
	# Third plot: difference of model, obs
	title = paste('[',model$name,'-',obs$name,'] ',varname,' ',plottype,sep='')
	errtext = PlotAus(obs$grid$lon,obs$grid$lat,(md$modelm-md$obsm),mean((md$modelm-md$obsm),na.rm=T),
		sd((md$modelm-md$obsm),na.rm=T),varname,unitstxt,longvarname,md$diffrange,diffcols,title,textloc)
	# Plot benchmark obs differences that exist (plots 4-6):
	if(bench$exist){
		for(b in 1:bench$howmany){
			title = paste('[',bench[[ bench$index[b] ]]$name,'-',obs$name,'] ',varname,' ',plottype,sep='')
			errtext = PlotAus(obs$grid$lon,obs$grid$lat,(md$benchm[[b]] - md$obsm),mean((md$benchm[[b]]-md$obsm),na.rm=T),
				sd((md$benchm[[b]]-md$obsm),na.rm=T),varname,unitstxt,longvarname,md$diffrange,diffcols,title,textloc)
		}
	}	
		
	### Add density insets ###
	mod_den = density(md$modelm,na.rm=TRUE) # calculate model density estimate 
	obs_den = density(md$obsm,na.rm=TRUE) # calculate obs density estimate
	xrange = DensityXrange(list(mod_den,obs_den),density_cut)
	# Plot pdfs for model and obs
	in1 = InsetDensity(density_location[[1]],mod_den,xrange)
	in2 = InsetDensity(density_location[[2]],obs_den,xrange)
	moderr_den = density((md$modelm-md$obsm),na.rm=TRUE) # calculate model error density estimate 
	xrange = DensityXrange(list(moderr_den),density_cut)
	if(bench$exist){
		bencherr_den = list()
		density_range_list = list(moderr_den)
		for(b in 1:bench$howmany){
			bencherr_den[[b]] = density((md$benchm[[b]]-md$obsm),na.rm=TRUE)
			density_range_list[[b+1]] = bencherr_den[[b]]
		}
		# If there's a benchmark, overwrite xrange to include benchmark info:
		xrange = DensityXrange(density_range_list,density_cut)
		for(b in 1:bench$howmany){
			inb = InsetDensity(density_location[[(b+3)]],bencherr_den[[b]],xrange)
		}
	}
	in3 = InsetDensity(density_location[[3]],moderr_den,xrange)
	
	result = list(errtext=errtext,err=FALSE,metrics=md$metrics)
	return(result)
}

SpatialAusRelative = function(model,obs,bench,md,variable,plottype){
	# Used for plots that show metrics that use model and obs together, e.g. RMSE model panel 
	# and RMSE panels for up to 3 benchmark-obs (i.e. max 4 panels).
	# Density plots are always in the lower left, mean and SD values are on the bottom right
	errtext = 'ok'
	varname = variable[['Name']][1]
	unitstxt = variable[['UnitsText']]
	longvarname = variable[['PlotName']]
	metrics = list()
	density_cut = 1/200 # truncate denisty plot x-axis at this fraction of max y value
	npanels = bench$howmany + 1 # Number of map panels
	
	# Plot layout:
	if(npanels == 1){
		density_location = DensityLocationAus(1)
		textloc='bottomright'
	}else if(npanels == 2){
		par(mfcol=c(1,2) ,mar=c(4,4,3,0.5),oma=c(6,0,5,1),mgp=c(2.5,0.7,0),ps=12,tcl=-0.4)
		density_location = DensityLocationAus(2)
		textloc='bottomright'
	}else if(npanels >= 3){
		par(mfcol=c(2,2) ,mar=c(3,3,3,0.5),oma=c(0,0,0,1),mgp=c(1.8,0.5,0),ps=15,tcl=-0.4)
		density_location = DensityLocationAus(4)
		textloc='bottomright'
	}
	
	# Fetch colour scheme:
	zcols = ChooseColours(md$zrange,varname,'positive')
	
	### Draw plot panels ###
	# First plot: model
	title = paste(model$name,' ',longvarname,' ',plottype,sep='')
	errtext = PlotAus(obs$grid$lon,obs$grid$lat,md$modelm,mean(md$modelm,na.rm=T),sd(md$modelm,na.rm=T),
		varname,unitstxt,longvarname,md$zrange,zcols,title,textloc,md$suppressunits)
	# Plot benchmarks that exist:
	if(bench$exist){
		for(b in 1:bench$howmany){
			title = paste(bench[[ bench$index[b] ]]$name,' ',longvarname,' ',plottype,sep='')
			errtext = PlotAus(obs$grid$lon,obs$grid$lat,md$benchm[[b]],mean(md$benchm[[b]],na.rm=T),
				sd(md$benchm[[b]],na.rm=T),varname,unitstxt,longvarname,md$zrange,zcols,
				title,textloc,md$suppressunits)
		}
	}	
		
	### Add density insets ###
	mod_den = density(md$modelm,na.rm=TRUE) # calculate model density estimate 
	xrange = DensityXrange(list(mod_den),density_cut) # truncate x-axis range
	if(bench$exist){
		bench_den = list()
		density_range_list = list(mod_den)
		for(b in 1:bench$howmany){
			bench_den[[b]] = density(md$benchm[[b]],na.rm=TRUE)
			density_range_list[[b+1]] = bench_den[[b]]
		}
		# If there's a benchmark, overwrite xrange to include benchmark info:
		xrange = DensityXrange(density_range_list,density_cut)
		for(b in 1:bench$howmany){
			inb = InsetDensity(density_location[[(b+1)]],bench_den[[b]],xrange)
		}
	}
	# Plot density inset for model metric
	in1 = InsetDensity(density_location[[1]],mod_den,xrange)	
		
	result = list(errtext=errtext,err=FALSE,metrics=md$metrics)
	return(result)
}

PlotAus = function(lon,lat,data,meanval,sdval,varname,unitstxt,longvarname,zrange,zcols,title,textloc,suppressunits=FALSE){
	# Generates a gridded heatmap style plot for Australia with map, based on input lat/lon.
	library(maps)
	library(mapdata)
	library(fields) # for image.plot
	errtext = 'ok'
	# Decide location of plot for text placement:
	if(textloc=='bottomright'){
		textloc1 = c((lon[1] + (lon[length(lon)] - lon[1])*0.9), (lat[1] + (lat[length(lat)] - lat[1])*0.19) )
		textloc2 = c((lon[1] + (lon[length(lon)] - lon[1])*0.9), (lat[1] + (lat[length(lat)] - lat[1])*0.13) )
		textloc3 = c((lon[1] + (lon[length(lon)] - lon[1])*0.9), (lat[1] + (lat[length(lat)] - lat[1])*0.07) )
	}else if(textloc=='topleft'){
		textloc1 = c((lon[1] + (lon[length(lon)] - lon[1])*0.15), (lat[1] + (lat[length(lat)] - lat[1])*0.94) )
		textloc2 = c((lon[1] + (lon[length(lon)] - lon[1])*0.15), (lat[1] + (lat[length(lat)] - lat[1])*0.88) )
		textloc3 = c((lon[1] + (lon[length(lon)] - lon[1])*0.15), (lat[1] + (lat[length(lat)] - lat[1])*0.82) )
	}
	# Plot:	
	image.plot(lon,lat,data,xlab='Longitude',ylab='Latitude',col=zcols,zlim=zrange,legend.mar=5.5)
	map('worldHires',add=TRUE,wrap=TRUE,xlim=c(min(lon),max(lon)),ylim=c(min(lat),max(lat))) # Add map
	title(title) 
	if(!suppressunits){
		text(x=textloc1[1],y=textloc1[2],labels=unitstxt)
	}
	text(x=textloc2[1],y=textloc2[2],labels=paste('Mean:',signif(meanval,3)))
	text(x=textloc3[1],y=textloc3[2],labels=paste('SD:',signif(sdval,2)))
	return(errtext)
}

DensityLocationAus = function(npanels){
	# Simply prescribes locations for density inset on gridded Australia map plots
	density_location = list()
	if(npanels == 1){
		density_location = list()
		density_location[[1]] = c(0.1,0.5,0.16,0.35)
	}else if(npanels == 2){
		density_location = list()
		density_location[[1]] = c(0.07,0.3,0.17,0.37)
		density_location[[2]] = c(0.57,0.8,0.17,0.37)
	}else if(npanels == 4){
		density_location = list()
		density_location[[1]] = c(0.06,0.3,0.59,0.68)
		density_location[[2]] = c(0.56,0.8,0.59,0.68)
		density_location[[3]] = c(0.06,0.3,0.09,0.18)
		density_location[[4]] = c(0.56,0.8,0.09,0.18)
	}else if(npanels==6){
		density_location = list()
		density_location[[1]] = c(0.04,0.2,0.58,0.67)
		density_location[[2]] = c(0.38,0.54,0.58,0.67)
		density_location[[3]] = c(0.72,0.87,0.58,0.67)
		density_location[[4]] = c(0.04,0.2,0.08,0.17)
		density_location[[5]] = c(0.38,0.54,0.08,0.17)
		density_location[[6]] = c(0.72,0.87,0.08,0.17)
	}
	return(density_location)
}

InsetDensity = function(location,densitydata,xrange){
	# Adds an inset density plot
	par(fig=location,new=T)
	plot(densitydata,lwd=3,main='',ylab='',xlab='',cex.axis=0.8,bty='n',mgp=c(2,0,0),yaxt='n',xlim=xrange,tcl=-0.2)
}

DensityXrange = function(density_list,density_cut){
	# Finds the x-axis range that contains all y values above a threshold
	# for a list of density functions.
	ymax = 0 # initialise
	xmin=NA # initialise
	xmax=NA # initialise
	for(d in 1:length(density_list)){
		ymax = max(ymax,density_list[[d]][[2]])	
	}
	# Determine where to truncate x-axis according to density cut threshold:
	for(d in 1:length(density_list)){
		xmin = min(xmin, density_list[[d]][[1]][ (density_list[[d]][[2]]>(ymax*density_cut)) ], na.rm=TRUE)
		xmax = max(xmax, density_list[[d]][[1]][ (density_list[[d]][[2]]>(ymax*density_cut)) ], na.rm=TRUE)
	}
	return(c(xmin,xmax))
}




