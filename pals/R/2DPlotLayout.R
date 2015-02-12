# 2DPlotLayout.R
#
# Gab Abramowitz, UNSW, 2015, gabsun at gmail dot com
#
SpatialPlotAbsolute = function(model,obs,bench,md,variable,plottype,region){
	# Layout of plots that show obs panel, model panel, model-obs panel and 
	# up to 3 benchmark-obs panels (i.e. max 6 panels).
	# Density plots are always in the lower left, mean and SD values are either bottom
	# right <= 4 panels, or top left for > 4 panels
	errtext = 'ok'
	varname = variable[['Name']][1]
	unitstxt = variable[['UnitsText']]
	longvarname = variable[['PlotName']]
	density_cut = 1/200 # truncate denisty plot x-axis at this fraction of max y value 
	npanels = bench$howmany + 3 # Number of map panels in plot
	
	# Plot layout:
	if(npanels <= 4){
		par(mfcol=c(2,2) ,mar=c(3,3,3,0.5),oma=c(0,0,0,1),mgp=c(1.8,0.5,0),ps=15,tcl=-0.4)
		density_location = DensityLocation(region,4)
		textloc = TextLocation(region,4)
	}else{
		par(mfcol=c(2,3) ,mar=c(3,3,3,0.5),oma=c(1,0,0.5,1),mgp=c(1.8,0.5,0),ps=18,tcl=-0.2)
		density_location = DensityLocation(region,6)
		textloc = TextLocation(region,6)
	}
	
	# Fetch colour scheme:
	zcols = ChooseColours(md$zrange,varname,'positive')
	diffcols = ChooseColours(md$diffrange,varname,'difference')
	
	### Draw plot panels ###
	# First plot: model	
	title = paste(model$name,' ',longvarname,' ',plottype,sep='')
	errtext = DrawPlot(region,obs$grid$lon,obs$grid$lat,md$modelm,mean(md$modelm,na.rm=T),sd(md$modelm,na.rm=T),
		varname,unitstxt,longvarname,md$zrange,zcols,title,textloc)
	# Second plot: obs 
	title = paste(obs$name,' ',longvarname,' ',plottype,sep='')
	errtext = DrawPlot(region,obs$grid$lon,obs$grid$lat,md$obsm,mean(md$obsm,na.rm=T),sd(md$obsm,na.rm=T),
		varname,unitstxt,longvarname,md$zrange,zcols,title,textloc)
	# Third plot: difference of model, obs
	title = paste('[',model$name,'-',obs$name,'] ',varname,' ',plottype,sep='')
	errtext = DrawPlot(region,obs$grid$lon,obs$grid$lat,(md$modelm-md$obsm),mean((md$modelm-md$obsm),na.rm=T),
		sd((md$modelm-md$obsm),na.rm=T),varname,unitstxt,longvarname,md$diffrange,diffcols,title,textloc)
	# Plot benchmark obs differences that exist (plots 4-6):
	if(bench$exist){
		for(b in 1:bench$howmany){
			title = paste('[',bench[[ bench$index[b] ]]$name,'-',obs$name,'] ',varname,' ',plottype,sep='')
			errtext = DrawPlot(region,obs$grid$lon,obs$grid$lat,(md$benchm[[b]] - md$obsm),mean((md$benchm[[b]]-md$obsm),na.rm=T),
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

SpatialPlotRelative = function(model,obs,bench,md,variable,plottype,region){
	# Layout of plots that show metrics that use model and obs together, e.g. RMSE model panel 
	# and RMSE panels for up to 3 benchmark-obs (i.e. max 4 panels).
	# Density plots are always in the lower left, mean and SD values are on the bottom right
	errtext = 'ok'
	varname = variable[['Name']][1]
	unitstxt = variable[['UnitsText']]
	longvarname = variable[['PlotName']]
	density_cut = 1/200 # truncate denisty plot x-axis at this fraction of max y value
	npanels = bench$howmany + 1 # Number of map panels
	
	# Plot layout:
	if(npanels == 1){
		density_location = DensityLocation(region,1)
		textloc=TextLocation(region,1)
	}else if(npanels == 2){
		par(mfcol=c(1,2) ,mar=c(4,4,3,0.5),oma=c(6,0,5,1),mgp=c(2.5,0.7,0),ps=12,tcl=-0.4)
		density_location = DensityLocation(region,2)
		textloc=TextLocation(region,2)
	}else if(npanels >= 3){
		par(mfcol=c(2,2) ,mar=c(3,3,3,0.5),oma=c(0,0,0,1),mgp=c(1.8,0.5,0),ps=15,tcl=-0.4)
		density_location = DensityLocation(region,4)
		textloc=TextLocation(region,4)
	}
	
	# Fetch colour scheme:
	zcols = ChooseColours(md$zrange,varname,'positive')
	
	### Draw plot panels ###
	# First plot: model
	title = paste(model$name,' ',longvarname,' ',plottype,sep='')
	errtext = DrawPlot(region,obs$grid$lon,obs$grid$lat,md$modelm,mean(md$modelm,na.rm=T),sd(md$modelm,na.rm=T),
		varname,unitstxt,longvarname,md$zrange,zcols,title,textloc,md$suppressunits)
	# Plot benchmarks that exist:
	if(bench$exist){
		for(b in 1:bench$howmany){
			title = paste(bench[[ bench$index[b] ]]$name,' ',longvarname,' ',plottype,sep='')
			errtext = DrawPlot(region,obs$grid$lon,obs$grid$lat,md$benchm[[b]],mean(md$benchm[[b]],na.rm=T),
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

DensityLocation = function(region,maxpanels){
	# Returns location within plot tiles of density functions
	if(region=='Australia'){
		result = DensityLocationAus(maxpanels)	
	}else if(region=='Global'){
		result = DensityLocationGlobal(maxpanels)
	}else{
		stop('region unknown!')	
	}
	return(result)
}

TextLocation = function(region,maxpanels){
	# Returns location within plot tiles of density functions
	if(region=='Australia'){
		result = TextLocationAus(maxpanels)	
	}else if(region=='Global'){
		result = TextLocationGlobal(maxpanels)
	}else{
		stop('region unknown!')	
	}
	return(result)
}

DrawPlot = function(region,lon,lat,data,meanval,sdval,varname,unitstxt,longvarname,
	zrange,zcols,title,textloc,suppressunits=FALSE){
	
	if(region=='Australia'){
		result = PlotAus(lon,lat,data,meanval,sdval,varname,unitstxt,longvarname,
			zrange,zcols,title,textloc,suppressunits)	
	}else if(region=='Global'){
		result = PlotGlobal(lon,lat,data,meanval,sdval,varname,unitstxt,longvarname,
			zrange,zcols,title,textloc,suppressunits)
	}
	else{
		stop('region unknown!')	
	}
	return(result)			
}


