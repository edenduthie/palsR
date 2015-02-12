# 2DMetrics.R
#
# Gab Abramowitz, UNSW, 2015, gabsun at gmail dot com
#
TimeMeanAll = function(model,obs,bench,variable,plottype){
	# Calculates time means for each grid point in obs, model and benchmark data  
	metrics = list()
	benchm = list()
	benchbias = c()
	# Calculate time means for plotting:
	modelm = TimeMean(model$data)
	obsm = TimeMean(obs$data) + modelm - modelm # to make sure ocean areas not included
	# Ranges of obs, model metric values (benchmarks only appear in difference plots):
	zrange = c(min(modelm,obsm,na.rm=TRUE),max(modelm,obsm,na.rm=TRUE))
	# Scalar metric for reporting:
	modelbias = mean(modelm-obsm,na.rm=TRUE)
	# Initial ranges for difference plots:
	dmax = max(modelm-obsm,na.rm=TRUE)
	dmin = min(modelm-obsm,na.rm=TRUE)
	if(bench$exist){
		for(b in 1:bench$howmany){
			# Get benchmark metric data, noting there may have been other benchmarks 
			# that failed (so use bench$index)
			benchm[[b]] = TimeMean(bench[[ bench$index[b] ]]$data)
			benchbias[b] = mean(benchm[[b]]-obsm,na.rm=TRUE)
			dmax = max(dmax,(benchm[[b]]-obsm),na.rm=TRUE)
			dmin = min(dmin,(benchm[[b]]-obsm),na.rm=TRUE)
		}
	}
	metrics[[1]] = list(name='TimeSpaceBias',model_value=modelbias,bench_value=benchbias)	
	result = list(modelm = modelm, obsm=obsm, benchm=benchm, diffrange = c(dmin, dmax), 
		zrange=zrange,metrics=metrics)
	return(result)
}

TimeSDAll = function(model,obs,bench,variable,plottype){
	# Calculates standard deviation for each grid point in obs, model and benchmark data
	metrics = list()
	benchm = list()
	benchSDbias = c()
	# Calculate time means:
	modelm = TimeSD(model$data)
	obsm = TimeSD(obs$data) + modelm - modelm # to make sure ocean areas not included
	# Ranges of obs, model metric values (benchmarks only appear in difference plots):
	zrange = c(min(modelm,obsm,na.rm=TRUE),max(modelm,obsm,na.rm=TRUE))
	# Scalar metric for reporting:
	modelSDbias = mean(modelm-obsm,na.rm=TRUE)
	# Initial ranges for difference plots:
	dmax = max(modelm-obsm,na.rm=TRUE)
	dmin = min(modelm-obsm,na.rm=TRUE)
	if(bench$exist){
		for(b in 1:bench$howmany){
			# Get benchmark metric data, noting there may have been other benchmarks 
			# that failed (so use bench$index)
			benchm[[b]] = TimeSD(bench[[ bench$index[b] ]]$data)
			benchSDbias[b] = mean(benchm[[b]]-obsm,na.rm=TRUE)
			dmax = max(dmax,(benchm[[b]]-obsm),na.rm=TRUE)
			dmin = min(dmin,(benchm[[b]]-obsm),na.rm=TRUE)
		}
	}
	metrics[[1]] = list(name='AvTimeSDbias',model_value=modelSDbias,bench_value=benchSDbias)	
	result = list(modelm = modelm, obsm=obsm, benchm=benchm, diffrange = c(dmin, dmax), 
		zrange=zrange,metrics=metrics)
	return(result)
}

TimeRMSEAll = function(model,obs,bench,variable,plottype){
	# Calculates root mean square error for each grid point for model and benchmark data
	metrics = list()
	benchm = list()
	benchRMSE = c()
	suppressunits = FALSE # i.e. RMSE has units - unlike, e.g. correlation
	# Calculate time RMSE for plotting:
	modelm = TimeRMSE(obs$data, model$data)
	modelRMSE = sqrt(mean((model$data - obs$data)^2,na.rm=TRUE)) # scalar reporting metric
	# Initial ranges:
	rmax = max(modelm,na.rm=TRUE)
	rmin = min(modelm,na.rm=TRUE)
	if(bench$exist){
		for(b in 1:bench$howmany){
			# Get benchmark metric data, noting there may have been other benchmarks 
			# that failed (so use bench$index)
			benchm[[b]] = TimeRMSE(obs$data, bench[[ bench$index[b] ]]$data)
			benchRMSE[b] = sqrt(mean((bench[[ bench$index[b] ]]$data - obs$data)^2,na.rm=TRUE))
			rmax = max(rmax,benchm[[b]],na.rm=TRUE)
			rmin = min(rmin,benchm[[b]],na.rm=TRUE)
		}
	}
	metrics[[1]] = list(name='TimeSpaceRMSE',model_value=modelRMSE,bench_value=benchRMSE)	
	result = list(modelm = modelm, benchm=benchm, zrange = c(rmin, rmax), 
		metrics=metrics, suppressunits = suppressunits)
	return(result)
}

TimeCorAll = function(model,obs,bench,variable,plottype){
	# Calculates correlation for each grid point for model,obs and benchmark,obs data
	metrics = list()  
	benchm = list()
	benchAvTimeCor = c()
	benchTimeSpaceCor = c()
	suppressunits = TRUE # i.e. correlation has no units
	# Calculate time correlation for plotting:
	modelm = TimeCor(obs$data, model$data)
	# Two scalar metrics:
	modelAvTimeCor = mean(modelm,na.rm=TRUE) # Average of time correlation
	modelTimeSpaceCor = cor(as.vector(obs$data),as.vector(model$data)) # Cor over time and space
	# initial ranges:
	rmax = max(modelm,na.rm=TRUE)
	rmin = min(modelm,na.rm=TRUE)
	if(bench$exist){
		for(b in 1:bench$howmany){
			# Get benchmark metric data, noting there may have been other benchmarks 
			# that failed (so use bench$index)
			benchm[[b]] = TimeCor(obs$data, bench[[ bench$index[b] ]]$data)
			benchAvTimeCor[b] = mean(benchm[[b]],na.rm=TRUE)
			benchTimeSpaceCor[b] = cor(as.vector(obs$data),as.vector(bench[[ bench$index[b] ]]$data))
			rmax = max(rmax,benchm[[b]],na.rm=TRUE)
			rmin = min(rmin,benchm[[b]],na.rm=TRUE)
		}
	}
	metrics[[1]] = list(name='AvTimeCor',model_value=modelAvTimeCor,bench_value=benchAvTimeCor)
	metrics[[2]] = list(name='TimeSpaceCor',model_value=modelTimeSpaceCor,bench_value=benchTimeSpaceCor)
	result = list(modelm = modelm, benchm=benchm, zrange = c(rmin, rmax), 
		metrics=metrics, suppressunits = suppressunits)
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