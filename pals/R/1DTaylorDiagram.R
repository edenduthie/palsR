# AnalysisTaylorDiagram.R
#
# Plots a Taylor diagram for a given modelled and observed variable.
#
# Gab Abramowitz UNSW 2014 palshelp@gmail.com
#
TaylorDiagram = function(data,varinfo,plotcolours){
	library(plotrix) # load package with Taylor diagram
	errtext = 'ok'
	# Get colours:
	plotcolours = BenchmarkColours(data$bench,plotobs=TRUE)
	plot2colours = DesaturateColours(plotcolours,sat=0.3)
	# Get text for legend:
	legendtext = LegendText(data,plotobs=TRUE)
	# First report metrics:
	metrics = list()
	if(data$bench$exist){
		if(data$bench$howmany == 1){
			metrics[[1]] = list(name='Correlation',
				model_value=cor(as.vector(data$model$data), as.vector(data$obs$data)),
				bench_value=list(
				bench1=cor(as.vector(data$bench[[ data$bench$index[1] ]]$data), as.vector(data$obs$data)) ))	
		}else if(data$bench$howmany == 2){
			metrics[[1]] = list(name='Correlation',
				model_value=cor(as.vector(data$model$data),as.vector(data$obs$data)),
				bench_value=list(
				bench1=cor(as.vector(data$bench[[ data$bench$index[1] ]]$data), as.vector(data$obs$data)),
				bench2=cor(as.vector(data$bench[[ data$bench$index[2] ]]$data), as.vector(data$obs$data)) ))
		}else if(data$bench$howmany == 3){
			metrics[[1]] = list(name='Correlation',
				model_value=cor(as.vector(data$model$data), as.vector(data$obs$data)),
				bench_value=list(
				bench1=cor(as.vector(data$bench[[ data$bench$index[1] ]]$data), as.vector(data$obs$data)),
				bench2=cor(as.vector(data$bench[[ data$bench$index[2] ]]$data), as.vector(data$obs$data)),
				bench3=cor(as.vector(data$bench[[ data$bench$index[3] ]]$data), as.vector(data$obs$data)) ))	
		}
	 }else{
		metrics[[1]] = list(name='Correlation',
			model_value=cor(as.vector(data$model$data),as.vector(data$obs$data)))
	}
	# Text for x-axis:
	xtext=bquote(.(varinfo$PlotName) ~ ' (' ~ .(varinfo$UnitsText) ~ ')')
	# First plot model on Taylor plot:
	taylor.diagram(ref=as.vector(data$obs$data),model=as.vector(data$model$data),xlab=xtext,pcex=2,
		main=paste(varinfo$name[1],' Taylor diagram:  Obs - ',data$obs$name,'  Mod - ',data$model$name, sep=''),
		ref.sd=TRUE,show.gamma=TRUE,cex.lab=1.2,cex.axis=1.1,cex.main=1.2,
		col=plotcolours[2],sd.arcs=TRUE)	
	# Then obs reference point on x-axis:
	taylor.diagram(ref=as.vector(data$obs$data),model=as.vector(data$obs$data),pcex=2,add=TRUE,
		ref.sd=TRUE,show.gamma=TRUE,col=plotcolours[1])
	# Then plot benchmarks:
	if(data$bench$exist){
		for(b in 1:data$bench$howmany){	
			taylor.diagram(ref=as.vector(data$obs$data),model=as.vector(data$bench[[ data$bench$index[b] ]]$data),
				pcex=2,add=TRUE,ref.sd=TRUE,show.gamma=TRUE,col=plotcolours[(b+2)])	
		}
	}
	
	###### Now calculate daily values ##############
	tstepinday=86400/data$obs$timing$tstepsize # number of time steps in a day
	# Reshape data into clolumn hours, day rows:
	mod_days=matrix(data$model$data,ncol=tstepinday,byrow=TRUE) 
	obs_days=matrix(data$obs$data,ncol=tstepinday,byrow=TRUE) 
	ndays=length(mod_days[,1]) # find number of days in data set
	nyears=as.integer(ndays/365) # find # years in data set
	mavday=c() # initialise
	oavday=c() # initialise
	for(i in 1:ndays){
		mavday[i] = mean(mod_days[i,])
		oavday[i] = mean(obs_days[i,])
	}
	# Reshape benchmark data:
	if(data$bench$exist){
		bavday=matrix(NA,data$bench$howmany,ndays) # initialise
		for(b in 1:data$bench$howmany){	
			bench_days=matrix(data$bench[[ data$bench$index[b] ]]$data,ncol=tstepinday,byrow=TRUE)
			for(i in 1:ndays){
				bavday[b,i] = mean(bench_days[i,])
			}
		}
	}	
	# Plot daily averages of model:
	taylor.diagram(ref=oavday,model=mavday,pcex=2,add=TRUE,
		ref.sd=TRUE,show.gamma=TRUE,col=plot2colours[2],pch=15)
	# Plot daily averages of obs reference circle:
	taylor.diagram(ref=oavday,model=oavday,pcex=2,add=TRUE,
		ref.sd=TRUE,show.gamma=TRUE,col=plot2colours[1],pch=15)
	# Plot daily averages of benchmarks:
	if(data$bench$exist){
		for(b in 1:data$bench$howmany){	
			taylor.diagram(ref=oavday,model=bavday[b,],pcex=2,add=TRUE,
				ref.sd=TRUE,show.gamma=TRUE,col=plot2colours[(b+2)],pch=15)	
		}
	}
	
	# # If we have whole years of data, plot monthly values as well
	# if(data$obs$timing$whole){
		# month = getMonthDays()
		# mod_monthly=c() # initialise monthly averages
		# obs_monthly=c()   # initialise monthly averages
		# # Transform daily means into monthly means:
		# for(l in 1:12){ # for each month
			# mod_month=0 # initialise
			# obs_month=0   # initialise
			# for(k in 1:nyears){ # for each year of data set
				# # Add all daily averages for a given month
				# # over all data set years:
				# mod_month = mod_month + 
					# sum(mavday[(month$start[l]+(k-1)*365):
					# (month$start[l+1]-1 +(k-1)*365) ] )
				# obs_month = obs_month + 
					# sum(oavday[(month$start[l]+(k-1)*365):
					# (month$start[l+1]-1 +(k-1)*365) ] )
			# }
			# # then divide by the total number of days added above:
			# mod_monthly[l]=mod_month/(month$length[l]*nyears)
			# obs_monthly[l]=obs_month/(month$length[l]*nyears)
		# }
		# # Plot monthly averages:
		# taylor.diagram(ref=obs_monthly,model=mod_monthly,pcex=2,add=TRUE,
		# ref.sd=TRUE,show.gamma=TRUE,col='green')
		# taylor.diagram(ref=obs_monthly,model=obs_monthly,pcex=2,add=TRUE,
		# ref.sd=TRUE,show.gamma=TRUE,col='green',pch=1)
		# # Add legend:
		# lpos = 1.2*max(sd(data$obs$data),sd(data$model$data))
		# legend(lpos,1.3*lpos,col=c('blue','red','green'),pch=19,
			# legend=c('per timestep','daily averages','monthly averages'))
	# }else{
	# Add legend:
	legend('topright',col=c(plotcolours,plot2colours[1]),
		pch=c(rep.int(19,(2+data$bench$howmany)),15),
		inset=c(-0.1,-0.05),legend=c(legendtext,'Daily averages'))		 
	# }
	result=list(err=FALSE,errtext=errtext,metrics=metrics)
	return(result)
} # End function TaylorDiagram