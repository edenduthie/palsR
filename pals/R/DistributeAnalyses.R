# DistributeAnalyses.R
#
# Functions to distribute analyses across multiple cores
#
# Gab Abramowitz, UNSW, 2014 (palshelp at gmail dot com)

DistributeGriddedAnalyses = function(Analysis,vars,obs,model,bench){
	# Each call to this function will generate a single plot and its statistics
	
	# Create outfilename:
	outfile = setOutput('default')
	
	# For now, assumes analysis will be for a single variable.
	# The name of this variable
	varname = vars[[Analysis$vindex]][['Name']][1]
	# Units expression:
	unitstxt = vars[[Analysis$vindex]][['UnitsText']]
	# Longer variable name for plots:
	longvarname = vars[[Analysis$vindex]][['PlotName']]
	# File name for graphics file:
	filestring = paste(getwd(),outfile,sep = "/")
	# Analysis identifier for javascript:
	outfiletype = paste(varname,tolower(Analysis$type))
	
	# Check obs or model aren't missing variable, their timing is compatible and grids match:
	errcheck = CanAnalysisProceed(obs, model)
	if(errcheck$err){
		result = list(type=outfiletype,filename=filestring,mimetype="image/png",
			error=errcheck$errtext,bencherror=bench$errtext,metrics=list(first=list(name='failed',model_value=NA)))
		return(result)
	}
	
	# Test benchmark timing compatibility, and remove any benchmarks if necessary:
	bench = PruneBenchmarks(obs,bench)
	
	# Call analysis function:	
	if(Analysis$type == 'TimeMean'){
		bencherrtext = bench$errtext
		areturn = SpatialAus(model,obs,bench,varname,unitstxt,longvarname,metrics,plottype=Analysis$type)				
	}else if(Analysis$type == 'TimeSD'){
		bencherrtext = bench$errtext
		areturn = SpatialAus(model,obs,bench,varname,unitstxt,longvarname,metrics,plottype=Analysis$type)
	}else if(Analysis$type == 'TimeRMSE'){
		bencherrtext = bench$errtext
		areturn = SpatialAusRelative(model,obs,bench,varname,unitstxt,longvarname,metrics,plottype=Analysis$type)		
	}else if(Analysis$type == 'TimeCor'){
		bencherrtext = bench$errtext
		areturn = SpatialAusRelative(model,obs,bench,varname,unitstxt,longvarname,metrics,plottype=Analysis$type)	
	}
	
	if(areturn$errtext=='ok'){	
		result = list(type=outfiletype,filename=paste(getwd(),outfile,sep = "/"),mimetype="image/png",
			metrics = areturn$metrics,analysistype=Analysis$type, variablename=varname,bencherror=bencherrtext)
	}else{
		cat('\n###',areturn$errtext,'###\n')
		result = list(type=outfiletype,filename=paste(getwd(),outfile,sep = "/"),mimetype="image/png",
			metrics = areturn$metrics,analysistype=Analysis$type, variablename=varname,
			error=areturn$errtext,bencherror=bencherrtext)
	}	
	
	return(result)
}

DistributeSingleSiteAnalyses = function(Analysis,data,vars){
		
	# Create outfilename:
	outfile = setOutput('ModelAnalysis')
	
	# First deal with multiple-variable analyses:
	if(Analysis$type == 'Conserve'){
		# No single variable, so 'multiple' returned in javascript:
		varname = 'multiple'
		# Analysis identifier for javascript:
		outfiletype = Analysis$type
	
	}else if(Analysis$type == 'EvapFrac'){
		# No single variable, so 'multiple' returned in javascript:
		varname = 'multiple'
		# Analysis identifier for javascript:
		outfiletype = Analysis$type
	
	}else{ # Analysis will be for a single variable.
		# The name of this variable
		varname = vars[[Analysis$vindex]][['Name']][1]
		# Units expression:
		unitstxt = vars[[Analysis$vindex]][['UnitsText']]
		# Longer variable name for plots:
		longvarname = vars[[Analysis$vindex]][['PlotName']]
		# File name for graphics file:
		filestring = paste(getwd(),outfile,sep = "/")
		# Analysis identifier for javascript:
		outfiletype = paste(varname,tolower(Analysis$type))
		
		# Check obs or model aren't missing variable data and and that their timing is compatible:
		errcheck = CanAnalysisProceed(data[[Analysis$vindex]]$obs,data[[Analysis$vindex]]$model)
		if(errcheck$err){
			result = list(type=outfiletype,filename=filestring,mimetype="image/png",analysistype=Analysis$type,
				error=errcheck$errtext,bencherror=data[[Analysis$vindex]]$bench$errtext,
				metrics=list(first=list(name='failed',model_value=NA)),variablename=varname)
			return(result)
		}
		
		# Test benchmark timing compatibility, and remove any benchmarks if necessary:
		data[[Analysis$vindex]]$bench = PruneBenchmarks(data[[Analysis$vindex]]$obs,data[[Analysis$vindex]]$bench)
		
		# Create data matrix to send to analysis function:
		adata=matrix(NA,length(data[[Analysis$vindex]]$obs$data),(2+data[[Analysis$vindex]]$bench$howmany))
		adata[,1] = data[[Analysis$vindex]]$obs$data
		adata[,2] = data[[Analysis$vindex]]$model$data
		# Add benchmark data, if any:
		if(data[[Analysis$vindex]]$bench$exist){
			for(b in 1: (data[[Analysis$vindex]]$bench$howmany) ){
				adata[,(b+2)] = data[[Analysis$vindex]]$bench[[ data[[Analysis$vindex]]$bench$index[b] ]]$data
			}
		}
		
		# Add obs quality control data, if present:		
		if(data[[Analysis$vindex]]$obs$qcexists){
			vqcdata = matrix(NA,length(data[[Analysis$vindex]]$obs$data),1)
			vqcdata[,1] = data[[Analysis$vindex]]$obs$qc
		}else{
			vqcdata = matrix(-1,nrow=1,ncol=1)
		}
		
		# For adding to plots:
		obsname = data[[Analysis$vindex]]$obs$name
		moname = data[[Analysis$vindex]]$model$name
		benchnames = c()
		if(data[[Analysis$vindex]]$bench$exist){
			for(b in 1: (data[[Analysis$vindex]]$bench$howmany) ){
				benchnames[b] = data[[Analysis$vindex]]$bench[[ data[[Analysis$vindex]]$bench$index[b] ]]$name
			}
		}
		
		legendtext = LegendText(data[[Analysis$vindex]],plotobs=TRUE)
		plotcolours = BenchmarkColours(data[[Analysis$vindex]]$bench,plotobs=TRUE)
		
		# Call analysis function:	
		if(Analysis$type == 'Timeseries'){
			bencherrtext = data[[Analysis$vindex]]$bench$errtext
			plotcex = 1.1 # plot text magnification factor
			winsize = 14
			ytext=bquote('Smoothed'~.(tolower(longvarname)) ~ ' (' ~ .(unitstxt) ~ ')')
			areturn = Timeseries(obsname,adata,varname,ytext,legendtext,plotcex,
				data[[Analysis$vindex]]$obs$timing,smoothed=TRUE,winsize,plotcolours,
				moname,vqcdata=vqcdata)				
		}else if(Analysis$type == 'AnnualCycle'){
			bencherrtext = data[[Analysis$vindex]]$bench$errtext
			ytext = bquote('Average'~.(tolower(longvarname)) ~ ' (' ~ .(unitstxt) ~ ')')
			areturn = AnnualCycle(obsname,adata,varname,ytext,legendtext,
				data[[Analysis$vindex]]$obs$timing$tstepsize,
				data[[Analysis$vindex]]$obs$timing$whole,plotcolours,moname)
		}else if(Analysis$type == 'DiurnalCycle'){
			bencherrtext = data[[Analysis$vindex]]$bench$errtext
			ytext=bquote('Average'~.(varname) ~ ' (' ~.(unitstxt) ~ ')')
			areturn = DiurnalCycle(obsname,adata,varname,ytext,legendtext,
				data[[Analysis$vindex]]$obs$timing$tstepsize,
				data[[Analysis$vindex]]$obs$timing$whole,plotcolours,moname,vqcdata=vqcdata)
		}else if(Analysis$type == 'PDF'){
			bencherrtext = data[[Analysis$vindex]]$bench$errtext
			nbins=500
			xtext=bquote(.(longvarname) ~ ' (' ~ .(unitstxt) ~ ')')
			areturn = PALSPdf(obsname,adata,varname,xtext,legendtext,
				data[[Analysis$vindex]]$obs$timing,nbins,plotcolours,moname,vqcdata=vqcdata)
		}else if(Analysis$type == 'Scatter'){
			bencherrtext = data[[Analysis$vindex]]$bench$errtext
			areturn = PALSScatter(data[[Analysis$vindex]],vars[[Analysis$vindex]],ebal=FALSE)
		}else if(Analysis$type == 'Taylor'){
			bencherrtext = data[[Analysis$vindex]]$bench$errtext
			areturn = TaylorDiagram(data[[Analysis$vindex]],vars[[Analysis$vindex]],plotcolours)
		}else if(Analysis$type == 'AvWindow'){
			# Not a benhcmark plot for the moment:
			bencherrtext = 'Benchmark analysis not available for this analysis type'
			ytext=bquote('Average'~.(longvarname) ~ .(unitstxt))
			areturn = AveragingWindow(obsname,moname,data[[Analysis$vindex]]$model$data,
				data[[Analysis$vindex]]$obs$data,varname,ytext,
				data[[Analysis$vindex]]$obs$timing$tstepsize)
		}
		
	}
	# Don't return errtext in output list unless there is an error - as requested by Eden
	if(areturn$errtext=='ok'){	
		result = list(type=outfiletype,filename=paste(getwd(),outfile,sep = "/"),mimetype="image/png",
			metrics = areturn$metrics,analysistype=Analysis$type, variablename=varname,
			bencherror=bencherrtext,obsname=obsname,moname=moname,benchnames=benchnames)
	}else{
		cat('\n###',areturn$errtext,'###\n')
		result = list(type=outfiletype,filename=paste(getwd(),outfile,sep = "/"),mimetype="image/png",
			metrics = areturn$metrics,analysistype=Analysis$type, variablename=varname,
			error=areturn$errtext,bencherror=bencherrtext,obsname=obsname,moname=moname,benchnames=benchnames)
	}	

	return(result)
}

