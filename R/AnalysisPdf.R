# AnalysisPdf.R
#
# Plots probability density functions
#
# Gab Abramowitz UNSW 2012 (palshelp at gmail dot com)

PALSPdf = function(obslabel,pdfdata,varname,xtext,legendtext,timing,
	nbins=500,modlabel='no',vqcdata=matrix(-1,nrow=1,ncol=1)){
	xcut = 1/100
	ncurves = length(pdfdata[1,]) # Number of curves in final plot:
	ntsteps = length(pdfdata[,1]) # Number of timesteps in data:
	tstepinday=86400/timing$tstepsize # number of time steps in a day
	ndays = ntsteps/tstepinday # number of days in data set
	plotcolours=getPlotColours() # in PALSconstants
	xmin = min(pdfdata)
	xmax = max(pdfdata)
	allden = list()
	allden[[1]]=density(pdfdata[,1],from=xmin,to=xmax,n=nbins)
	heights = matrix(0,ncurves,nbins)
	heights[1,] = allden[[1]][[2]]
	ymax = max(heights[1,])
	if(vqcdata[1,1] != -1){
		qcden = density(pdfdata[as.logical(vqcdata[,1]),1],from=xmin,to=xmax,n=nbins)
		ymax = max(ymax,qcden[[1]][[2]])
	}
	# Find highest density value:
	if(ncurves>1){
		for(p in 2:ncurves){ # for each additional curve
			allden[[p]] = density(pdfdata[,p],from=xmin,to=xmax,n=nbins)
			ymax = max(ymax,allden[[p]][[2]])
			heights[p,] = allden[[p]][[2]]
		}
	}
	# Determine where x axis cutoffs should be:
	for(b in 1:nbins){
		if(max(heights[,b]) > ymax*xcut){
			xlow = allden[[1]][[1]][b]
			break
		}	
	}
	for(b in nbins:1){
		if(max(heights[,b]) > ymax*xcut){
			xhigh = allden[[1]][[1]][b]
			break
		}	
	}
	# Draw density plot:
	plot(allden[[1]],xlab=xtext,ylab='Density',main='',col=plotcolours[1],lwd=4,
		ylim=c(0,ymax),xlim=c(xlow,xhigh))
	# Plot pdf with gap-filled data removed, if info available:
	if(vqcdata[1,1] != -1){
		lines(qcden,col='gray35',lwd=2)
	}
	overl = c()
	if(ncurves>1){
		for(p in 2:ncurves){ # for each additional curve
			lines(allden[[p]],col=plotcolours[p],lwd=3)
			# Calculate overlap with observational pdf:
			intersection=c()
			for(b in 1:nbins){
				intersection[b]=min(allden[[1]][[2]][b],allden[[p]][[2]][b])
			}
			if(ncurves==2){
				polygon(c(allden[[1]][[1]][1],allden[[1]][[1]],allden[[1]][[1]][nbins]),
					c(0,intersection,0),col="grey")
			}
			# Calcualte obs-model overlap area:
			overl[p-1] = signif(sum(intersection)*(xmax-xmin)/nbins*100,2)
		}
		scoretext = paste(overl,collapse=', ')
		text(x=(xlow+(xhigh-xlow)*0.75),y=ymax*0.6,labels=paste('Overlap: ',scoretext,'%',sep=''),pos=4)
	}
	if(vqcdata[1,1] != -1){
		ltext = c(legendtext[1])
		lcols = c(plotcolours[1])
		llwd = c(4)
		ltext[2] = '(Obs no gapfill)'
		lcols[2] = 'grey35'
		llwd[2] = 2
		if(ncurves>1){
			for(p in 2:ncurves){
				ltext[p+1] = legendtext[p]
				lcols[p+1] = plotcolours[p]
				llwd[p+1] = 3
			}
		}
		legend(xlow+(xhigh-xlow)*0.8,ymax,legend=ltext,lty=1,
			col=lcols,lwd=llwd,bty="n")
	}else{
		legend(xlow+(xhigh-xlow)*0.8,ymax,legend=legendtext[1:ncurves],lty=1,
			col=plotcolours[1:ncurves],lwd=3,bty="n")
	}
	if(modlabel=='no'){
		title(paste(varname[1],' density:  Obs - ',obslabel,sep=''))
	}else{
		title(paste(varname[1],' density:   Obs - ',obslabel,'   Model - ',modlabel,sep=''))
	}
}

BenchPdf = function(analysisType,varname,units,xtext,legendtext){
	nbins=500
	checkUsage(analysisType)
	setOutput(analysisType)
	# Load model and obs data:
	obs = GetFluxnetVariable(varname,getObservedFluxDataFilePath(analysisType),units)
	# Load model data:
	model = GetModelOutput(varname,getModelOutputFilePath(analysisType),units)
	# Check compatibility between model and obs (same dataset):
	CheckTiming(model$timing,obs$timing)
	# Load benchmark names and paths:
	UserBenchPaths = getUserBenchPaths()
	UserBenchNames = getUserBenchNames()
	nbench = length(UserBenchNames) # number of benchmarks
	bench = matrix(NA,nbench,length(obs$data))
	# Load benchmark data:
	for(b in 1:nbench){
		if(substr(UserBenchNames[b],1,5)=='B_Emp'){ 
			# this is an empirical benchmark
			bvarname = paste(varname[1],'_',
				substr(UserBenchNames[b],6,nchar(UserBenchNames[b])),sep='')
			tmp_flx = GetBenchmarkVariable(bvarname,UserBenchPaths[b])
			# Check emp benchmark is based on same data set and version as obs:
			if(b==1){
				CheckVersionCompatibility(UserBenchPaths[b],
					getObservedFluxDataFilePath(analysisType))
			}
		}else{
			# this benchmark is a model simulation
			tmp_flx = GetModelOutput(varname,UserBenchPaths[b],units)
		}
		# Check benchmark data timing is compatible with obs:
		if(b==1){
			CheckTiming(tmp_flx$timing,obs$timing)
		}
		# For now, if requested benchmark variable doesn't exist in benchmark
		# file, stop script:
		if(is.null(tmp_flx)){
			CheckError(paste('B4: Could not find benchmark variable "',bvarname,
				'" in benchmark netcdf file.',sep=''))	
		}
		bench[b,] = tmp_flx$data
	}
	# Create data matrix for function:
	pdfdata=matrix(NA,length(model$data),(2+nbench))
	pdfdata[,1] = obs$data
	pdfdata[,2] = model$data
	for(b in 1:nbench){
		pdfdata[,(b+2)] = bench[b,]
	}
	# Check if obs QC/gap-filling data exists, and if so, send to plotting function:
	if(obs$qcexists){
		vqcdata = matrix(NA,length(obs$data),1)
		vqcdata[,1] = obs$qc
		# Call Timeseries plotting function:
		PALSPdf(getObsLabel(analysisType),pdfdata,varname,xtext,
			legendtext,obs$timing,nbins,getModLabel(analysisType),vqcdata=vqcdata)
	}else{
		# Call Timeseries plotting function:
		PALSPdf(getObsLabel(analysisType),pdfdata,varname,xtext,
			legendtext,obs$timing,nbins,getModLabel(analysisType))
	}
}

ModelPdf = function(analysisType,varname,units,xtext,legendtext){
	nbins=500
	checkUsage(analysisType)
	setOutput(analysisType)
	# Load model and obs data:
	obs = GetFluxnetVariable(varname,getObservedFluxDataFilePath(analysisType),units)
	# Load model data:
	model = GetModelOutput(varname,getModelOutputFilePath(analysisType),units)
	# Check compatibility between model and obs (same dataset):
	CheckTiming(model$timing,obs$timing)
	# Create data matrix for function:
	pdfdata=matrix(NA,length(model$data),2)
	pdfdata[,1] = obs$data
	pdfdata[,2] = model$data
	# Check if obs QC/gap-filling data exists, and if so, send to plotting function:
	if(obs$qcexists){
		vqcdata = matrix(NA,length(obs$data),1)
		vqcdata[,1] = obs$qc
		# Call Timeseries plotting function:
		PALSPdf(getObsLabel(analysisType),pdfdata,varname,xtext,
			legendtext,obs$timing,nbins,getModLabel(analysisType),vqcdata=vqcdata)
	}else{
		# Call Timeseries plotting function:
		PALSPdf(getObsLabel(analysisType),pdfdata,varname,xtext,
			legendtext,obs$timing,nbins,getModLabel(analysisType))
	}
}

ObsPdf = function(analysisType,varname,units,xtext,legendtext){
	nbins=500
	checkUsage(analysisType)
	setOutput(analysisType)
	# Load obs data:
	obs = GetFluxnetVariable(varname,getObservedFluxDataFilePath(analysisType),units)
	# Create data matrix for function:
	tsdata=matrix(NA,length(obs$data),1)
	tsdata[,1] = obs$data
	# Call Timeseries plotting function:
	PALSPdf(getObsLabel(analysisType),tsdata,varname,xtext,
		legendtext,obs$timing,nbins)
}