# 2DMetrics.R
#
# Gab Abramowitz, UNSW, 2015, gabsun at gmail dot com
#
TimeMeanAll = function(model,obs,bench,variable,plottype){
	benchm = list()
	benchbias = c()
	# Calculate time means:
	modelm = TimeMean(model$data)
	obsm = TimeMean(obs$data) + modelm - modelm # to make sure ocean areas not included
	modelbias = mean(modelm-obsm,na.rm=TRUE)
	# initial ranges:
	rmax = max(modelm-obsm,na.rm=TRUE)
	rmin = min(modelm-obsm,na.rm=TRUE)
	if(bench$exist){
		for(b in 1:bench$howmany){
			# Get benchmark metric data, noting there may have been other benchmarks 
			# that failed (so use bench$index)
			benchm[[b]] = TimeMean(bench[[ bench$index[b] ]]$data)
			benchbias[b] = mean(benchm[[b]]-obsm,na.rm=TRUE)
			rmax = max(rmax,(benchm[[b]]-obsm),na.rm=TRUE)
			rmin = min(rmin,(benchm[[b]]-obsm),na.rm=TRUE)
		}
	}
	metrics = list(name='TimeSpaceBias',model_value=modelbias,bench_value=benchbias)	
	result = list(modelm = modelm, obsm=obsm, benchm=benchm, diffrange = c(rmin, rmax), metrics=metrics)
	return(result)
}

TimeSDAll = function(model,obs,bench,variable,plottype){
	benchm = list()
	benchSDbias = c()
	# Calculate time means:
	modelm = TimeSD(model$data)
	obsm = TimeSD(obs$data) + modelm - modelm # to make sure ocean areas not included
	modelSDbias = mean(modelm-obsm,na.rm=TRUE)
	# initial ranges:
	rmax = max(modelm-obsm,na.rm=TRUE)
	rmin = min(modelm-obsm,na.rm=TRUE)
	if(bench$exist){
		for(b in 1:bench$howmany){
			# Get benchmark metric data, noting there may have been other benchmarks 
			# that failed (so use bench$index)
			benchm[[b]] = TimeSD(bench[[ bench$index[b] ]]$data)
			benchSDbias[b] = mean(benchm[[b]]-obsm,na.rm=TRUE)
			rmax = max(rmax,(benchm[[b]]-obsm),na.rm=TRUE)
			rmin = min(rmin,(benchm[[b]]-obsm),na.rm=TRUE)
		}
	}
	metrics = list(name='AvTimeSDbias',model_value=modelSDbias,bench_value=benchSDbias)	
	result = list(modelm = modelm, obsm=obsm, benchm=benchm, diffrange = c(rmin, rmax), metrics=metrics)
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