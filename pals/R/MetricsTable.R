# Collates metrics from analysis script into a PNG table
# Gab Abramowitz, UNSW, 2014 (palshelp at gmail dot com)

MetricTableSingleSite = function(outinfo,nbench){
	# Draws in a single image:
	# summary table: just number of 'wins' for model and all benchmarks
	# relationship analysis table: e.g. evapFrac; water use efficiency
	# variable analysis table: metrics that are repeated for each analysis type
	library(plotrix)
	
	# Create analysis name for javascript:
	thisanalysistype = 'Summary'
	
	#### Create variable analysis table ####
	ctr = 0
	analysistype = c()
	variable = c()
	metricname = c()
	model = c()
	bench1 = c()
	bench2 = c()
	bench3 = c()
	# Collect vectors for table data frame:
	for(i in 1: length(outinfo)){ # for each analysis performed
		if(outinfo[[i]]$variablename != 'multiple'){ # i.e. this is a single variable analysis
			# Number of metrics for this analysis:
			nmetrics = length(outinfo[[i]]$metrics)
			for(m in 1:nmetrics){
				if(outinfo[[i]]$metrics[[m]]$name != 'failed'){
					ctr = ctr + 1 # start index for this analysis
					analysistype[ctr] = outinfo[[i]]$analysistype
					variable[ctr] = outinfo[[i]]$variablename
					metricname[ctr] = outinfo[[i]]$metrics[[m]]$name
					model[ctr] = outinfo[[i]]$metrics[[m]]$model_value
					bench1[ctr] = NA
					bench2[ctr] = NA
					bench3[ctr] = NA
					if((nbench > 0) && (! is.null(outinfo[[i]]$metrics[[m]]$bench_value$bench1))){
						bench1[ctr] = outinfo[[i]]$metrics[[m]]$	bench_value$bench1		
					}
					if((nbench > 1) && (! is.null(outinfo[[i]]$metrics[[m]]$bench_value$bench2))){
						bench2[ctr] = outinfo[[i]]$metrics[[m]]$	bench_value$bench2
					}
					if((nbench > 2) && (! is.null(outinfo[[i]]$metrics[[m]]$bench_value$bench3))){
						bench3[ctr] = outinfo[[i]]$metrics[[m]]$	bench_value$bench3
					}
				}
			}
		}	
	}
	# Create data frame with vars*analyses rows:
	dfall = data.frame(analysis=analysistype,variable=variable,metric=metricname,
		mvalue=model,b1value=bench1,b2value=bench2,b3value=bench3)
	# Create vector of unique variables:
	vars = unique(dfall[,'variable'])
	halfvars = ceiling(length(vars)/2) # used to split table
	split_table = FALSE
	# Define row names:
	dfrownames = paste(dfall[variable==vars[1],][,1], dfall[variable==vars[1],][,3])
	ctr = 1
	# First entry in final table data frame is model metric value for var 1
	dffinal = data.frame(a=signif(dfall[variable==vars[1],][,4],3))
	dfcols = c(paste('Mod',vars[1]))
	if(!all(is.na(dfall[variable==vars[1],][,5]))){
		ctr = ctr + 1
		dffinal[ctr] = signif(dfall[variable==vars[1],][,5],3) # benchmark 1 metric value for var 1
		dfcols = c(dfcols,paste('B1',vars[1]))
	}
	if(!all(is.na(dfall[variable==vars[1],][,6]))){
		ctr = ctr + 1
		dffinal[ctr] = signif(dfall[variable==vars[1],][,6],3) # benchmark 2 metric value for var 1
		dfcols = c(dfcols,paste('B2',vars[1]))
	}
	if(!all(is.na(dfall[variable==vars[1],][,7]))){
		ctr = ctr + 1
		dffinal[ctr] = signif(dfall[variable==vars[1],][,7],3) # benchmark 3 metric value for var 1
		dfcols = c(dfcols,paste('B3',vars[1]))
	}
	if(length(vars) > 1){
		for(v in 2:length(vars)){
			ctr = ctr + 1
			dffinal[ctr] = signif(dfall[variable==vars[v],][,4],3)
			dfcols = c(dfcols,paste('Mod',vars[v]))
			if(!all(is.na(dfall[variable==vars[v],][,5]))){
				ctr = ctr + 1
				dffinal[ctr] = signif(dfall[variable==vars[v],][,5],3)
				dfcols = c(dfcols,paste('B1',vars[v]))
			}
			if(!all(is.na(dfall[variable==vars[v],][,6]))){
				ctr = ctr + 1
				dffinal[ctr] = signif(dfall[variable==vars[v],][,6],3)
				dfcols = c(dfcols,paste('B2',vars[v]))
			}
			if(!all(is.na(dfall[variable==vars[v],][,7]))){
				ctr = ctr + 1
				dffinal[ctr] = signif(dfall[variable==vars[v],][,7],3)
				dfcols = c(dfcols,paste('B3',vars[v]))
			}
			# Decide whether table should be split:
			if((v==halfvars) && (ctr > 5)){
				halftable = ctr + 1	
				split_table = TRUE
			}
		}
	}
	colnames(dffinal) = dfcols
	rownames(dffinal) = dfrownames
	
	# Produce table grob...
	outfile = setOutput('ModelAnalysis') # First set output
	# Create blank plot with size:
	plot(1:10, axes = FALSE, xlab = "", ylab = "", type = "n")
	if(split_table){
		addtable2plot(0,5,dffinal[1:(halftable-1)],bty='o',hlines=TRUE,vlines=TRUE,
			display.rownames=TRUE)
		addtable2plot(0,1,dffinal[halftable:length(dffinal)],bty='o',hlines=TRUE,
			vlines=TRUE,display.rownames=TRUE)
	}else{	
		addtable2plot(0,7,dffinal,bty='o',hlines=TRUE,vlines=TRUE,display.rownames=TRUE)
	}
	print(dffinal)	
			
	result = list(type=thisanalysistype,filename=paste(getwd(),outfile,sep = "/"),
		mimetype="image/png",analysistype=thisanalysistype)
	outinfo[[(length(outinfo)+1)]] = result
	return(outinfo)
}


# make metrics into matrix / data frame

# use grid.table with data frame

# or heatmap.2

# or addtable2plot

# or color2D.matplot