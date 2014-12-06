# 2DAusMean.R
#
# Gab Abramowitz, UNSW, 2014, gabsun at gmail dot com
#
SpatialAus = function(model,obs,bench,varname,unitstxt,longvarname,metrics,plottype){
	errtext = 'ok'
	metrics = list()
	density_cut = 1/200
	# Calculate number of map panels:
	npanels = bench$howmany + 3
	# Plot layout:
	if(npanels <= 4){
		par(mfcol=c(2,2) ,mar=c(3,3,3,0.5),oma=c(0,0,0,1),mgp=c(1.8,0.5,0),ps=15,tcl=-0.4)
		density_location = DensityLocationAus(4)
		textloc='bottomright'
	}else{
		par(mfcol=c(2,3) ,mar=c(3,3,3,0.5),oma=c(1,0,0.5,1),mgp=c(1.8,0.5,0),ps=18,tcl=-0.2)
		density_location = list()
		density_location = DensityLocationAus(6)
		textloc='topleft'
	}
	if(plottype=='TimeMean'){
		# Calculate time means:
		modelt = TimeMean(model$data)
		obst = TimeMean(obs$data) + modelt - modelt # to make sure ocean areas not included
		modelbias = mean(modelt-obst,na.rm=TRUE)
	}else if(plottype=='TimeSD'){
		modelt = TimeSD(model$data)
		obst = TimeSD(obs$data) + modelt - modelt # to make sure ocean areas not included
		modelSDbias = mean(modelt-obst,na.rm=TRUE)
	}else{
		result = list(errtext = paste('Unknown plot type \'',plottype,'\' requested in function SpatialAus.',sep=''),err=TRUE)
		return(result)
	}
	zrange = c(min(modelt,obst,na.rm=TRUE),max(modelt,obst,na.rm=TRUE))
	diffrange = c(min(modelt-obst,na.rm=TRUE),max(modelt-obst,na.rm=TRUE))
	if(plottype=='TimeMean'){
		if(bench$howmany == 1){
			# Get benchmark data, noting there may have been other benchmarks that failed (so use bench$index)
			bench1t = TimeMean(bench[[ bench$index[1] ]]$data)
			bench1bias = mean(bench1t-obst,na.rm=TRUE)
			diffrange = c(min(modelt-obst,bench1t-obst,na.rm=TRUE),
				max(modelt-obst,bench1t-obst,na.rm=TRUE))
			metrics[[1]] = list(name='TimeSpaceBias',model_value=modelbias,bench_value=list(bench1=bench1bias))	
		}else if(bench$howmany == 2){
			# Get benchmark data, noting there may have been other benchmarks that failed (so use bench$index)
			bench1t = TimeMean(bench[[ bench$index[1] ]]$data)
			bench2t = TimeMean(bench[[ bench$index[2] ]]$data)
			bench1bias = mean(bench1t-obst,na.rm=TRUE)
			bench2bias = mean(bench2t-obst,na.rm=TRUE)
			diffrange = c(min(modelt-obst,bench1t-obst,bench2t-obst,na.rm=TRUE),
				max(modelt-obst,bench1t-obst,bench2t-obst,na.rm=TRUE))
			metrics[[1]] = list(name='TimeSpaceBias',model_value=modelbias,bench_value=list(bench1=bench1bias,bench2=bench2bias))
		}else if(bench$howmany == 3){
			bench1t = TimeMean(bench[[ bench$index[1] ]]$data)
			bench2t = TimeMean(bench[[ bench$index[2] ]]$data)
			bench3t = TimeMean(bench[[ bench$index[3] ]]$data)
			bench1bias = mean(bench1t-obst,na.rm=TRUE)
			bench2bias = mean(bench2t-obst,na.rm=TRUE)
			bench3bias = mean(bench3t-obst,na.rm=TRUE)
			diffrange = c(min(modelt-obst,bench1t-obst,bench2t-obst,bench3t-obst,na.rm=TRUE),
				max(modelt-obst,bench1t-obst,bench2t-obst,bench3t-obst,na.rm=TRUE))
			metrics[[1]] = list(name='TimeSpaceBias',model_value=modelbias,
				bench_value=list(bench1=bench1bias,bench2=bench2bias,bench3=bench3bias))
		}else{
			# Just save metric list for model:
			metrics[[1]] = list(name='TimeSpaceBias',model_value=modelbias)	
		}
	}else if(plottype=='TimeSD'){
		if(bench$howmany == 1){
			# Get benchmark data, noting there may have been other benchmarks that failed (so use bench$index)
			bench1t = TimeSD(bench[[ bench$index[1] ]]$data)
			bench1SDbias = mean(bench1t-obst,na.rm=TRUE)
			diffrange = c(min(modelt-obst,bench1t-obst,na.rm=TRUE),
				max(modelt-obst,bench1t-obst,na.rm=TRUE))
			metrics[[1]] = list(name='AvTimeSDbias',model_value=modelSDbias,bench_value=list(bench1=bench1SDbias))	
		}else if(bench$howmany == 2){
			# Get benchmark data, noting there may have been other benchmarks that failed (so use bench$index)
			bench1t = TimeSD(bench[[ bench$index[1] ]]$data)
			bench2t = TimeSD(bench[[ bench$index[2] ]]$data)
			bench1SDbias = mean(bench1t-obst,na.rm=TRUE)
			bench2SDbias = mean(bench2t-obst,na.rm=TRUE)
			diffrange = c(min(modelt-obst,bench1t-obst,bench2t-obst,na.rm=TRUE),
				max(modelt-obst,bench1t-obst,bench2t-obst,na.rm=TRUE))
			metrics[[1]] = list(name='AvTimeSDbias',model_value=modelSDbias,bench_value=list(bench1=bench1SDbias,bench2=bench2SDbias))
		}else if(bench$howmany == 3){
			bench1t = TimeSD(bench[[ bench$index[1] ]]$data)
			bench2t = TimeSD(bench[[ bench$index[2] ]]$data)
			bench3t = TimeSD(bench[[ bench$index[3] ]]$data)
			bench1SDbias = mean(bench1t-obst,na.rm=TRUE)
			bench2SDbias = mean(bench2t-obst,na.rm=TRUE)
			bench3SDbias = mean(bench3t-obst,na.rm=TRUE)
			diffrange = c(min(modelt-obst,bench1t-obst,bench2t-obst,bench3t-obst,na.rm=TRUE),
				max(modelt-obst,bench1t-obst,bench2t-obst,bench3t-obst,na.rm=TRUE))
			metrics[[1]] = list(name='AvTimeSDbias',model_value=modelSDbias,
				bench_value=list(bench1=bench1SDbias,bench2=bench2SDbias,bench3=bench3SDbias))
		}else{
			# Just save metric list for model:
			metrics[[1]] = list(name='AvTimeSDbias',model_value=modelSDbias)	
		}
	}
	# Fetch colour scheme:
	zcols = ChooseColours(zrange,varname,'positive')
	diffcols = ChooseColours(diffrange,varname,'difference')
	
	# First plot: model	mean
	title = paste(model$name,' ',longvarname,' ',plottype,sep='')
	errtext = PlotAus(obs$lon,obs$lat,modelt,mean(modelt,na.rm=T),sd(modelt,na.rm=T),
		varname,unitstxt,longvarname,zrange,zcols,title,textloc)
	# Second plot: obs mean
	title = paste(obs$name,' ',longvarname,' ',plottype,sep='')
	errtext = PlotAus(obs$lon,obs$lat,obst,mean(obst,na.rm=T),sd(obst,na.rm=T),
		varname,unitstxt,longvarname,zrange,zcols,title,textloc)
	# Third plot: difference of model, obs mean
	title = paste('[',model$name,'-',obs$name,'] ',varname,' ',plottype,sep='')
	errtext = PlotAus(obs$lon,obs$lat,(modelt-obst),mean((modelt-obst),na.rm=T),
		sd((modelt-obst),na.rm=T),varname,unitstxt,longvarname,diffrange,diffcols,title,textloc)
	# Plot benchmarks that exist:
	if(bench$exist){
		# Fourth plot: difference bench1, obs 
		title = paste('[',bench[[ bench$index[1] ]]$name,'-',obs$name,'] ',varname,' ',plottype,sep='')
		errtext = PlotAus(obs$lon,obs$lat,(bench1t - obst),mean((bench1t-obst),na.rm=T),
			sd((bench1t-obst),na.rm=T),varname,unitstxt,longvarname,diffrange,diffcols,title,textloc)
		if(bench$howmany >= 2){
			# Fifth plot: difference bench2, obs 
			title = paste('[',bench[[ bench$index[2] ]]$name,'-',obs$name,'] ',varname,' ',plottype,sep='')
			errtext = PlotAus(obs$lon,obs$lat,(bench2t - obst),mean((bench2t-obst),na.rm=T),
				sd((bench2t-obst),na.rm=T),varname,unitstxt,longvarname,diffrange,diffcols,title,textloc)
			if(bench$howmany == 3){	
				# Fifth plot: difference bench3, obs 
				title = paste('[',bench[[ bench$index[3] ]]$name,'-',obs$name,'] ',varname,' ',plottype,sep='')
				errtext = PlotAus(obs$lon,obs$lat,(bench3t - obst),mean((bench3t-obst),na.rm=T),
					sd((bench3t-obst),na.rm=T),varname,unitstxt,longvarname,diffrange,diffcols,title,textloc)
			}
		}	
	}
	### Add density insets ###
	mod_den = density(modelt,na.rm=TRUE) # calculate model density estimate 
	obs_den = density(obst,na.rm=TRUE) # calculate obs density estimate
	xrange = DensityXrange(list(mod_den,obs_den),density_cut)
	# Plot pdfs for model and obs
	in1 = InsetDensity(density_location[[1]],mod_den,xrange)
	in2 = InsetDensity(density_location[[2]],obs_den,xrange)
	moderr_den = density((modelt-obst),na.rm=TRUE) # calculate model error density estimate 
	xrange = DensityXrange(list(moderr_den),density_cut)
	if(bench$howmany == 1){
		bench1err_den = density((bench1t-obst),na.rm=TRUE)
		# If there's a benchmark, overwrite xrange to include benchmark info:
		xrange = DensityXrange(list(moderr_den,bench1err_den),density_cut)
		in4 = InsetDensity(density_location[[4]],bench1err_den,xrange)
	}else if(bench$howmany == 2){
		bench1err_den = density((bench1t-obst),na.rm=TRUE)
		bench2err_den = density((bench2t-obst),na.rm=TRUE)
		xrange = DensityXrange(list(moderr_den,bench1err_den,bench2err_den),density_cut)
		in4 = InsetDensity(density_location[[4]],bench1err_den,xrange)	
		in5 = InsetDensity(density_location[[5]],bench2err_den,xrange)	
	}else if(bench$howmany == 3){
		bench1err_den = density((bench1t-obst),na.rm=TRUE)
		bench2err_den = density((bench2t-obst),na.rm=TRUE)
		bench3err_den = density((bench3t-obst),na.rm=TRUE)
		xrange = DensityXrange(list(moderr_den,bench1err_den,bench2err_den,bench3err_den),density_cut)
		in4 = InsetDensity(density_location[[4]],bench1err_den,xrange)	
		in5 = InsetDensity(density_location[[5]],bench2err_den,xrange)
		in6 = InsetDensity(density_location[[6]],bench3err_den,xrange)		
	}
	in3 = InsetDensity(density_location[[3]],moderr_den,xrange)
	result = list(errtext=errtext,err=FALSE,metrics=metrics)
	return(result)
}
SpatialAusRelative = function(model,obs,bench,varname,unitstxt,longvarname,metrics,plottype){
	errtext = 'ok'
	metrics = list()
	density_cut = 1/200
	# Calculate number of map panels:
	npanels = bench$howmany + 1
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
	if(plottype=='TimeRMSE'){
		# Calculate time means:
		modelt = TimeRMSE(obs$data, model$data)
		modelRMSE = sqrt(mean((model$data - obs$data)^2,na.rm=TRUE))
		supressunits = FALSE
	}else if(plottype=='TimeCor'){
		modelt = TimeCor(obs$data, model$data)
		modelAvTimeCor = mean(modelt,na.rm=TRUE)
		modelTimeSpaceCor = cor(as.vector(obs$data),as.vector(model$data))
		supressunits = TRUE
	}else{
		result = list(errtext = paste('Unknown plot type \'',plottype,'\' requested in function SpatialAusRelative.',sep=''),err=TRUE)
		return(result)
	}
	zrange = c(min(modelt,na.rm=TRUE),max(modelt,na.rm=TRUE))
	if(plottype=='TimeRMSE'){
		if(bench$howmany == 1){
			# Get benchmark data, noting there may have been other benchmarks that failed (so use bench$index)
			bench1t = TimeRMSE(obs$data, bench[[ bench$index[1] ]]$data)
			bench1RMSE = sqrt(mean((bench[[ bench$index[1] ]]$data - obs$data)^2,na.rm=TRUE))
			metrics[[1]] = list(name='TimeSpaceRMSE',model_value=modelRMSE,bench_value=list(bench1=bench1RMSE))
			zrange = c(min(modelt,bench1t,na.rm=TRUE),max(modelt,bench1t,na.rm=TRUE))
		}else if(bench$howmany == 2){
			# Get benchmark data, noting there may have been other benchmarks that failed (so use bench$index)
			bench1t = TimeRMSE(obs$data, bench[[ bench$index[1] ]]$data)
			bench2t = TimeRMSE(obs$data, bench[[ bench$index[2] ]]$data)
			bench1RMSE = sqrt(mean((bench[[ bench$index[1] ]]$data - obs$data)^2,na.rm=TRUE))
			bench2RMSE = sqrt(mean((bench[[ bench$index[2] ]]$data - obs$data)^2,na.rm=TRUE))
			metrics[[1]] = list(name='TimeSpaceRMSE',model_value=modelRMSE,
				bench_value=list(bench1=bench1RMSE,bench2=bench2RMSE))
			zrange = c(min(modelt,bench1t,bench2t,na.rm=TRUE),max(modelt,bench1t,bench2t,na.rm=TRUE))
		}else if(bench$howmany == 3){
			bench1t = TimeRMSE(obs$data, bench[[ bench$index[1] ]]$data)
			bench2t = TimeRMSE(obs$data, bench[[ bench$index[2] ]]$data)
			bench3t = TimeRMSE(obs$data, bench[[ bench$index[3] ]]$data)
			bench1RMSE = sqrt(mean((bench[[ bench$index[1] ]]$data - obs$data)^2,na.rm=TRUE))
			bench2RMSE = sqrt(mean((bench[[ bench$index[2] ]]$data - obs$data)^2,na.rm=TRUE))
			bench3RMSE = sqrt(mean((bench[[ bench$index[3] ]]$data - obs$data)^2,na.rm=TRUE))
			metrics[[1]] = list(name='TimeSpaceRMSE',model_value=modelRMSE,
				bench_value=list(bench1=bench1RMSE,bench2=bench2RMSE,bench3=bench3RMSE))
			zrange = c(min(modelt,bench1t,bench2t,bench3t,na.rm=TRUE),max(modelt,bench1t,bench2t,bench3t,na.rm=TRUE))
		}else{
			# Just save metric list for model:
			metrics[[1]] = list(name='TimeSpaceRMSE',model_value=modelRMSE)
		}
	}else if(plottype=='TimeCor'){
		if(bench$howmany == 1){
			# Get benchmark data, noting there may have been other benchmarks that failed (so use bench$index)
			bench1t = TimeCor(obs$data, bench[[ bench$index[1] ]]$data)
			bench1AvTimeCor = mean(bench1t,na.rm=TRUE)
			bench1TimeSpaceCor = cor(as.vector(obs$data),as.vector(bench[[ bench$index[1] ]]$data))
			metrics[[1]] = list(name='AvTimeCor',model_value=modelAvTimeCor,bench_value=list(bench1=bench1AvTimeCor))
			metrics[[2]] = list(name='TimeSpaceCor',model_value=modelTimeSpaceCor,bench_value=list(bench1=bench1TimeSpaceCor))
			zrange = c(min(modelt,bench1t,na.rm=TRUE),max(modelt,bench1t,na.rm=TRUE))
		}else if(bench$howmany == 2){
			# Get benchmark data, noting there may have been other benchmarks that failed (so use bench$index)
			bench1t = TimeCor(obs$data, bench[[ bench$index[1] ]]$data)
			bench2t = TimeCor(obs$data, bench[[ bench$index[2] ]]$data)
			bench1AvTimeCor = mean(bench1t,na.rm=TRUE)
			bench1TimeSpaceCor = cor(as.vector(obs$data),as.vector(bench[[ bench$index[1] ]]$data))
			bench2AvTimeCor = mean(bench2t,na.rm=TRUE)
			bench2TimeSpaceCor = cor(as.vector(obs$data),as.vector(bench[[ bench$index[2] ]]$data))
			metrics[[1]] = list(name='AvTimeCor',model_value=modelAvTimeCor,
				bench_value=list(bench1=bench1AvTimeCor,bench2=bench2AvTimeCor))
			metrics[[2]] = list(name='TimeSpaceCor',model_value=modelTimeSpaceCor,
				bench_value=list(bench1=bench1TimeSpaceCor,bench2=bench2TimeSpaceCor))
			zrange = c(min(modelt,bench1t,bench2t,na.rm=TRUE),max(modelt,bench1t,bench2t,na.rm=TRUE))
		}else if(bench$howmany == 3){
			bench1t = TimeCor(obs$data, bench[[ bench$index[1] ]]$data)
			bench2t = TimeCor(obs$data, bench[[ bench$index[2] ]]$data)
			bench3t = TimeCor(obs$data, bench[[ bench$index[3] ]]$data)
			bench1AvTimeCor = mean(bench1t,na.rm=TRUE)
			bench1TimeSpaceCor = cor(as.vector(obs$data),as.vector(bench[[ bench$index[1] ]]$data))
			bench2AvTimeCor = mean(bench2t,na.rm=TRUE)
			bench2TimeSpaceCor = cor(as.vector(obs$data),as.vector(bench[[ bench$index[2] ]]$data))
			bench3AvTimeCor = mean(bench3t,na.rm=TRUE)
			bench3TimeSpaceCor = cor(as.vector(obs$data),as.vector(bench[[ bench$index[3] ]]$data))
			metrics[[1]] = list(name='AvTimeCor',model_value=modelAvTimeCor,
				bench_value=list(bench1=bench1AvTimeCor,bench2=bench2AvTimeCor,bench3=bench3AvTimeCor))
			metrics[[2]] = list(name='TimeSpaceCor',model_value=modelTimeSpaceCor,
				bench_value=list(bench1=bench1TimeSpaceCor,bench2=bench2TimeSpaceCor,bench3=bench3TimeSpaceCor))
			zrange = c(min(modelt,bench1t,bench2t,bench3t,na.rm=TRUE),max(modelt,bench1t,bench2t,bench3t,na.rm=TRUE))
		}else{
			# Just save metric list for model:
			metrics[[1]] = list(name='AvTimeCor',model_value=modelAvTimeCor)
			metrics[[2]] = list(name='TimeSpaceCor',model_value=modelTimeSpaceCor)
		}
	}
	# Fetch colour scheme:
	zcols = ChooseColours(zrange,varname,'positive')
	
	# First plot: model
	title = paste(model$name,' ',longvarname,' ',plottype,sep='')
	errtext = PlotAus(obs$lon,obs$lat,modelt,mean(modelt,na.rm=T),sd(modelt,na.rm=T),
		varname,unitstxt,longvarname,zrange,zcols,title,textloc,supressunits)
	# Plot benchmarks that exist:
	if(bench$exist){
		# Second plot: bench1
		title = paste(bench[[ bench$index[1] ]]$name,' ',longvarname,' ',plottype,sep='')
		errtext = PlotAus(obs$lon,obs$lat,bench1t,mean(bench1t,na.rm=T),
			sd(bench1t,na.rm=T),varname,unitstxt,longvarname,zrange,zcols,title,textloc,supressunits)
		if(bench$howmany >= 2){
			# Third plot: bench2
			title = paste(bench[[ bench$index[2] ]]$name,' ',longvarname,' ',plottype,sep='')
			errtext = PlotAus(obs$lon,obs$lat,bench2t,mean(bench2t,na.rm=T),
				sd(bench2t,na.rm=T),varname,unitstxt,longvarname,zrange,zcols,title,textloc,supressunits)
			if(bench$howmany == 3){	
				# Fourth plot: bench3
				title = paste(bench[[ bench$index[3] ]]$name,' ',longvarname,' ',plottype,sep='')
				errtext = PlotAus(obs$lon,obs$lat,bench3t,mean(bench3t,na.rm=T),
					sd(bench3t,na.rm=T),varname,unitstxt,longvarname,zrange,zcols,title,textloc,supressunits)
			}
		}	
	}
	### Add density insets ###
	mod_den = density(modelt,na.rm=TRUE) # calculate model density estimate 
	xrange = DensityXrange(list(mod_den),density_cut) # truncate x-axis range
	if(bench$howmany == 1){
		bench1_den = density(bench1t,na.rm=TRUE)
		# If there's a benchmark, overwrite xrange to include benchmark info:
		xrange = DensityXrange(list(mod_den,bench1_den),density_cut)
		in2 = InsetDensity(density_location[[2]],bench1_den,xrange)
	}else if(bench$howmany == 2){
		bench1_den = density(bench1t,na.rm=TRUE)
		bench2_den = density(bench2t,na.rm=TRUE)
		xrange = DensityXrange(list(mod_den,bench1_den,bench2_den),density_cut)
		in2 = InsetDensity(density_location[[2]],bench1_den,xrange)	
		in3 = InsetDensity(density_location[[3]],bench2_den,xrange)	
	}else if(bench$howmany == 3){
		bench1_den = density(bench1t,na.rm=TRUE)
		bench2_den = density(bench2t,na.rm=TRUE)
		bench3_den = density(bench3t,na.rm=TRUE)
		xrange = DensityXrange(list(mod_den,bench1_den,bench2_den,bench3_den),density_cut)
		in2 = InsetDensity(density_location[[2]],bench1_den,xrange)	
		in3 = InsetDensity(density_location[[3]],bench2_den,xrange)
		in4 = InsetDensity(density_location[[4]],bench3_den,xrange)		
	}
	# Plot pdfs for model
	in1 = InsetDensity(density_location[[1]],mod_den,xrange)
	result = list(errtext=errtext,err=FALSE,metrics=metrics)
	return(result)
}

TimeMean = function(threedvar){
	# Take the time mean of 3D variable
	twodvar = apply(threedvar,c(1,2),mean)	
	return(twodvar)
}
TimeSD = function(threedvar){
	# Take the time sd of 3D variable
	twodvar = apply(threedvar,c(1,2),sd)	
	return(twodvar)
}
TimeRMSE = function(obs3d,model3d){
	 twodvar = apply((model3d - obs3d),c(1,2),rootmeansquare)
	 return(twodvar)
}
rootmeansquare = function(diffvector){
	result = sqrt(mean(diffvector^2))
	return(result)
}
TimeCor = function(obs3d,model3d){
#		omean = TimeMean(obs3d)
#		mmean = TimeMean(model3d)
#		scov = apply(((obs3d - omean)*(model3d-mmean)),c(1,2),sum)
#		twodcor = scov / (TimeSD(obs3d) * TimeSD(model3d))
		twodcor = matrix(NA,length(obs3d[,1,1]),length(obs3d[1,,1]))
		for(i in 1:length(obs3d[,1,1])){
			for(j in 1:length(obs3d[1,,1])){
				twodcor[i,j] = cor(obs3d[i,j,],model3d[i,j,])
			}
		}				
	 return(twodcor)
}

PlotAus = function(lon,lat,data,meanval,sdval,varname,unitstxt,longvarname,zrange,zcols,title,textloc,supressunits=FALSE){
	# Generates a gridded heatmap style plot for Australia with map, based on input lat/lon.
	library(maps)
	library(mapdata)
	library(fields) # for image.plot
	errtext = 'ok'
	# Decide location of plot for text placement:
	if(textloc=='bottomright'){
		textloc1 = c((obs$lon[1] + (obs$lon[length(obs$lon)] - obs$lon[1])*0.9), (obs$lat[1] + (obs$lat[length(obs$lat)] - obs$lat[1])*0.19) )
		textloc2 = c((obs$lon[1] + (obs$lon[length(obs$lon)] - obs$lon[1])*0.9), (obs$lat[1] + (obs$lat[length(obs$lat)] - obs$lat[1])*0.13) )
		textloc3 = c((obs$lon[1] + (obs$lon[length(obs$lon)] - obs$lon[1])*0.9), (obs$lat[1] + (obs$lat[length(obs$lat)] - obs$lat[1])*0.07) )
	}else if(textloc=='topleft'){
		textloc1 = c((obs$lon[1] + (obs$lon[length(obs$lon)] - obs$lon[1])*0.15), (obs$lat[1] + (obs$lat[length(obs$lat)] - obs$lat[1])*0.94) )
		textloc2 = c((obs$lon[1] + (obs$lon[length(obs$lon)] - obs$lon[1])*0.15), (obs$lat[1] + (obs$lat[length(obs$lat)] - obs$lat[1])*0.88) )
		textloc3 = c((obs$lon[1] + (obs$lon[length(obs$lon)] - obs$lon[1])*0.15), (obs$lat[1] + (obs$lat[length(obs$lat)] - obs$lat[1])*0.82) )
	}
	# Plot:	
	image.plot(lon,lat,data,xlab='Longitude',ylab='Latitude',col=zcols,zlim=zrange,legend.mar=5.5)
	map('worldHires',add=TRUE,wrap=TRUE,xlim=c(min(lon),max(lon)),ylim=c(min(lat),max(lat))) # Add map
	title(title) 
	if(!supressunits){
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




