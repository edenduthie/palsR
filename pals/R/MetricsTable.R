# Collates metrics from analysis script into a PNG table
# Gab Abramowitz, UNSW, 2014 (palshelp at gmail dot com)

MetricTableSingleSite = function(outinfo,BenchInfo){
	# Draws in a single image:
	# summary table: just number of 'wins' for model and all benchmarks
	# relationship analysis table: e.g. evapFrac; water use efficiency
	# variable analysis table: metrics that are repeated for each analysis type
	library(plotrix)
	
	# Create analysis name for javascript:
	thisanalysistype = 'Summary'
	
	# Number of significant figures in table data:
	sfig = 2
	# Table spacing:
	xpadding = 0.2
	ypadding = 1.0
	# Table text sizing:
	tablecex = 0.95
	# Table grid:
	xpos=c(-0.3,5,8)
	ypos=c(5.3,0)
	
	# First get obs, model and benchmark names:
	obsname = outinfo[[1]]$obsname
	moname = outinfo[[1]]$moname
	##### ************ get names from javascript read in instead!
	benchnames = BenchInfo$names
	
	# Then collect model and benchmark metric value vectors for all analyses into a data frame.
	# Initialisations:
	ctr = 0
	analysistype = c()
	variable = c()
	metricname = c()
	model = c()
	bench1 = c()
	bench2 = c()
	bench3 = c()
	winner = c()
	# Collate metric data:
	for(i in 1: length(outinfo)){ # for each analysis performed
		if(outinfo[[i]]$variablename != 'multiple'){ # i.e. this is a single variable analysis		
			nmetrics = length(outinfo[[i]]$metrics) # Number of metrics for this analysis
			for(m in 1:nmetrics){				
				if(outinfo[[i]]$metrics[[m]]$name != 'failed'){ # if analysis was successful
					# First get obs DS, MO name for plot title (need only happen once, overwritten):
					obsname = outinfo[[i]]$obsname
					moname = outinfo[[i]]$moname
					
					ctr = ctr + 1 # start index for this analysis
					analysistype[ctr] = outinfo[[i]]$analysistype
					variable[ctr] = outinfo[[i]]$variablename
					metricname[ctr] = outinfo[[i]]$metrics[[m]]$name
					model[ctr] = outinfo[[i]]$metrics[[m]]$model_value
					# Initialise benchmark metric values for this analysis:
					bench1[ctr] = NA
					bench2[ctr] = NA
					bench3[ctr] = NA
					# Note that bench 1 in any given analysis in outinfo may not correspond to the user's
					# first nominated benchmark - a relevant variable may be missing from the benchmark MO,
					# or there may have been an error reading. The AlignBenchmarks function below deals with this:
					if(BenchInfo$number >= 1){
						bench1[ctr] = 
							AlignBenchmarks(benchnames,outinfo[[i]]$benchnames,1,outinfo[[i]]$metrics[[m]]$bench_value)
					}
					if(BenchInfo$number >= 2){
						bench2[ctr] = 
							AlignBenchmarks(benchnames,outinfo[[i]]$benchnames,2,outinfo[[i]]$metrics[[m]]$bench_value)
					}
					if(BenchInfo$number >= 3){
						bench3[ctr] = 
							AlignBenchmarks(benchnames,outinfo[[i]]$benchnames,3,outinfo[[i]]$metrics[[m]]$bench_value)
					}
					# Note whether model or bench 1, 2 or 3 'won' this metric:
					winner[ctr] = BestInMetric(metricname[ctr],model[ctr],bench1[ctr],bench2[ctr],bench3[ctr])
				}
			}
		}	
	}
	# Create data frame with vars*analyses rows:
	dfall = data.frame(analysis=analysistype,variable=variable,metric=metricname,
		mvalue=model,b1value=bench1,b2value=bench2,b3value=bench3,winner=winner)
		
	# Create vector of unique variable names:
	vars = unique(dfall[,'variable'])
	halfvars = ceiling(length(vars)/2) # used to split table
	split_table = FALSE
	# Define row names:
	initnames = paste(dfall[variable==vars[1],][,3],' (',dfall[variable==vars[1],][,1],')',sep='')
	dfrownames = PadRowNames(initnames)
	ctr = 1
	vctr = c() # initialise index of columns for each variable in final data frame
	
	# First (column) entry in final table data frame is model metric values (for all analyses) for var 1:
	dffinal = data.frame(a=signif(dfall[variable==vars[1],][,4],sfig))
	dfcolumns = c(paste('Mod',vars[1]))
	# Create colours data frame and add first vector:
	dfcolours = data.frame(row1 = ifelse(c((dfall[variable==vars[1],][,8]) == 1),TableWinColour(),TableModelColour()))
	if(!all(is.na(dfall[variable==vars[1],][,5]))){ # if any bench1 values for var 1 (for all analyses) exist
		ctr = ctr + 1
		dffinal[ctr] = signif(dfall[variable==vars[1],][,5],sfig) # benchmark 1 metric values for var 1
		dfcolumns = c(dfcolumns,'B1')
		dfcolours[ctr] = ifelse(c((dfall[variable==vars[1],][,8]) == 2),TableWinColour(),TableBenchmarkColour())
	}
	if(!all(is.na(dfall[variable==vars[1],][,6]))){
		ctr = ctr + 1
		dffinal[ctr] = signif(dfall[variable==vars[1],][,6],sfig) # benchmark 2 metric values for var 1
		dfcolumns = c(dfcolumns,'B2')
		dfcolours[ctr] = ifelse(c((dfall[variable==vars[1],][,8]) == 3),TableWinColour(),TableBenchmarkColour())
	}
	if(!all(is.na(dfall[variable==vars[1],][,7]))){
		ctr = ctr + 1
		dffinal[ctr] = signif(dfall[variable==vars[1],][,7],sfig) # benchmark 3 metric values for var 1
		dfcolumns = c(dfcolumns,'B3')
		dfcolours[ctr] = ifelse(c((dfall[variable==vars[1],][,8]) == 4),TableWinColour(),TableBenchmarkColour())
	}
	vctr[1] = ctr
	if(length(vars) > 1){
		for(v in 2:length(vars)){
			ctr = ctr + 1
			dffinal[ctr] = signif(dfall[variable==vars[v],][,4],sfig)
			dfcolumns = c(dfcolumns,paste('Mod',vars[v]))
			dfcolours[ctr] = ifelse(c((dfall[variable==vars[v],][,8]) == 1),TableWinColour(),TableModelColour())
			if(!all(is.na(dfall[variable==vars[v],][,5]))){
				ctr = ctr + 1
				dffinal[ctr] = signif(dfall[variable==vars[v],][,5],sfig)
				dfcolumns = c(dfcolumns,'B1')
				dfcolours[ctr] = ifelse(c((dfall[variable==vars[v],][,8]) == 2),TableWinColour(),TableBenchmarkColour())
			}
			if(!all(is.na(dfall[variable==vars[v],][,6]))){
				ctr = ctr + 1
				dffinal[ctr] = signif(dfall[variable==vars[v],][,6],sfig)
				dfcolumns = c(dfcolumns,'B2')
				dfcolours[ctr] = ifelse(c((dfall[variable==vars[v],][,8]) == 3),TableWinColour(),TableBenchmarkColour())
			}
			if(!all(is.na(dfall[variable==vars[v],][,7]))){
				ctr = ctr + 1
				dffinal[ctr] = signif(dfall[variable==vars[v],][,7],sfig)
				dfcolumns = c(dfcolumns,'B3')
				dfcolours[ctr] = ifelse(c((dfall[variable==vars[v],][,8]) == 4),TableWinColour(),TableBenchmarkColour())
			}
			vctr[v] = ctr
			# Decide whether table should be split:
			if((v==halfvars) && (ctr > 5)){
				halftable = ctr + 1	
				split_table = TRUE
			}
		}
	}
	# Add column and row names to data frame:
	colnames(dffinal) = dfcolumns
	rownames(dffinal) = dfrownames
	# Produce table grob...
	outfile = setOutput('ModelAnalysis') # First set output
	# Create blank plot with size:
	plot(1:10, axes = FALSE, xlab = "", ylab = "", type = "n")
	# Title and subtitle:
	if(BenchInfo$number>0){
		title_line2 = paste('Model: ',moname,'    Benchmarks: ',paste(benchnames,collapse=', '))
	}else{
		title_line2 = paste('Model:',moname)
	}
	title(main=paste(obsname,'metric summary'))
	mtext(title_line2,cex=1.1,col='blue4')
	# Draw table:
	if(split_table){
		addtable2plot(xpos[1],ypos[1],dffinal[1:vctr[1]],bty='o',hlines=TRUE,vlines=TRUE,cex=tablecex,
			display.rownames=TRUE,bg=as.matrix(dfcolours)[,1:vctr[1]],xpad=xpadding,ypad=ypadding)
		for(v in 2:3){
			addtable2plot(xpos[v],ypos[1],dffinal[(vctr[v-1]+1):(vctr[v])],bty='o',hlines=TRUE,vlines=TRUE,cex=tablecex,
				display.rownames=FALSE,bg=as.matrix(dfcolours)[,(vctr[v-1]+1):(vctr[v])],xpad=xpadding,ypad=ypadding)	
		}
		addtable2plot(xpos[1],ypos[2],dffinal[(vctr[3]+1):vctr[4]],bty='o',hlines=TRUE,vlines=TRUE,cex=tablecex,
			display.rownames=TRUE,bg=as.matrix(dfcolours)[,(vctr[3]+1):vctr[4]],xpad=xpadding,ypad=ypadding)
		for(v in 5:length(vars)){
			addtable2plot(xpos[v%%3],ypos[2],dffinal[(vctr[v-1]+1):(vctr[v])],bty='o',hlines=TRUE,vlines=TRUE,cex=tablecex,
				display.rownames=FALSE,bg=as.matrix(dfcolours)[,(vctr[v-1]+1):(vctr[v])],xpad=xpadding,ypad=ypadding)	
		}
	}else{
		addtable2plot(xpos[1],7,dffinal[1:vctr[1]],bty='o',hlines=TRUE,vlines=TRUE,cex=tablecex,
			display.rownames=TRUE,bg=as.matrix(dfcolours)[,1:vctr[1]],xpad=xpadding,ypad=ypadding)
		for(v in 2:length(vars)){
			addtable2plot(xpos[v],7,dffinal[(vctr[v-1]+1):(vctr[v])],bty='o',hlines=TRUE,vlines=TRUE,cex=tablecex,
				display.rownames=FALSE,bg=as.matrix(dfcolours)[,(vctr[v-1]+1):(vctr[v])],xpad=xpadding,ypad=ypadding)	
		}
		
		addtable2plot(-0.2,7,dffinal,bty='o',hlines=TRUE,vlines=TRUE,display.rownames=TRUE,cex=tablecex,
			bg=as.matrix(dfcolours),xpad=xpadding,ypad=ypadding)
	}
		
	# Return analysis result by adding to outinfo list of analyses results.		
	result = list(type=thisanalysistype,filename=paste(getwd(),outfile,sep = "/"),
		mimetype="image/png",analysistype=thisanalysistype)
	outinfo[[(length(outinfo)+1)]] = result
	return(outinfo)
}

AlignBenchmarks = function(UserBenchNames,AnalysisBenchNames,CurrentUserBench,BenchValues){
	# For a given user nominated benchmark - CurrentUserBench - this function determines what the
	# metric value of the benchmark is. In particular, when a user-nominated benchmark is not utlised for
	# an analysis (variable missing / error), the user-nominated list of benchmarks becomes out of sync
	# with the analysis list of benchmarks. This function ensures that the user-nominated benchmark metric 
	# value is allocated appropriately in such cases.
	
	#print(UserBenchNames)
	#print(AnalysisBenchNames)
	#print(CurrentUserBench)
	#print(BenchValues)
	
	if(CurrentUserBench == 1){
		# Either user nominate benchamrk is analysis benchmark 1, or it failed:
		if(is.na(AnalysisBenchNames[1])){ # i.e. all analyses failed (no 1st benchmark in analysis)
			result = NA
		}else if(UserBenchNames[1] == AnalysisBenchNames[1]){
			result = BenchValues$bench1
		}else{ # just the first user nominated benchmark failed
			result = NA
		}
	}else if(CurrentUserBench == 2){
		if(is.na(AnalysisBenchNames[1])){ # i.e. all analyses failed (no 1st benchmark in analysis)
			result = NA
		}else if(UserBenchNames[2] == AnalysisBenchNames[1]){
			result = BenchValues$bench1
		}else if(is.na(AnalysisBenchNames[2])){ # i.e. no 2nd benchmark in analysis
			result = NA
		}else if(UserBenchNames[2] == AnalysisBenchNames[2]){
			result = BenchValues$bench2
		}else{ # the 2nd user nominated benchmark failed
			result = NA
		}
	}else if(CurrentUserBench == 3){
		if(is.na(AnalysisBenchNames[1])){ # i.e. all analyses failed (no 1st benchmark in analysis)
			result = NA
		}else if(UserBenchNames[3] == AnalysisBenchNames[1]){
			result = BenchValues$bench1
		}else if(is.na(AnalysisBenchNames[2])){ # i.e. no 2nd benchmark in analysis
			result = NA
		}else if(UserBenchNames[3] == AnalysisBenchNames[2]){
			result = BenchValues$bench2
		}else if(is.na(AnalysisBenchNames[3])){ # i.e. no 3rd benchmark in analysis
			result = NA	
		}else if(UserBenchNames[3] == AnalysisBenchNames[3]){
			result = BenchValues$bench3
		}
	}
	
	if(is.null(result)){ # e.g. if analysis does not return a value for a benchmark
		result = NA	
	}
	
	return(result)
}

BestInMetric = function(name,model,bench1,bench2,bench3){
	# Returns an integer that indicates whether the submitting LSM (1), 
	# the first (2), second (3), or third (4) nominated benchmark was
	# best in a particular metric.
	# If there are no benchmark values, or no metric value is given, there is no winner and 0 is returned
	if(is.na(bench1) | grepl('nometric',tolower(name))){
		return(0)
	}
	if(grepl('nme',tolower(name)) | grepl('rmse',tolower(name))){
		result = which.min(c(model,bench1,bench2,bench3))
	}else if(grepl('grad',tolower(name)) | grepl('gradient',tolower(name))){
		result = which.min(abs(1-c(model,bench1,bench2,bench3)))
	}else if(grepl('pdf',tolower(name)) | grepl('overlap',tolower(name)) | 
		grepl('correlation',tolower(name)) | grepl('cor',tolower(name))){
		result = which.max(c(model,bench1,bench2,bench3))
	}else if(grepl('int',tolower(name)) | grepl('intercept',tolower(name))){
		result = which.min(abs(c(model,bench1,bench2,bench3)))
	}else if(grepl('bias',tolower(name))){
		result = which.min(abs(c(model,bench1,bench2,bench3)))
	}
	return(result)
}

PadRowNames = function(initnames){
	len = max(nchar(initnames))
	result = initnames
	for(n in 1:length(initnames)){
		addchar = len - nchar(initnames[n])
		if(addchar != 0){
			result[n] = paste(' ',initnames[n],sep='')
			if(addchar > 1){
				for(a in 1:length(addchar)){
					result[n] = paste(' ',result[n],sep='')
				}
			}
		}
	}
	return(result)
}

# make metrics into matrix / data frame

# use grid.table with data frame

# or heatmap.2

# or addtable2plot

# or color2D.matplot