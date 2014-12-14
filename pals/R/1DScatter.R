# AnalysisScatter.R
#
# Plots a scatterplot of model vs obs
#
# Gab Abramowitz UNSW 2014 (palshelp at gmail dot com)
#
PALSScatter = function(data,varinfo,ebal=FALSE){
	#
	errtext = 'ok'
	metrics = list()
	ntsteps = length(data$obs$data) # Number of timesteps in data
	tstepinday=86400/data$obs$timing$tstepsize # number of time steps in a day
	ndays = ntsteps/tstepinday # number of days in data set
	# Plot layout:
	par(mfcol=c(1,2),mar=c(4,4,3,0.5),oma=c(0,0,0,1),
		mgp=c(2.5,0.7,0),ps=14,tcl=-0.4)
	# Get plot colours, adjuted for any removed benchmarks:
	plotcols = BenchmarkColours(data$bench,plotobs=FALSE)
	# Get legend text:
	legendtext = LegendText(data,plotobs=FALSE)
	# Define legend line types:
	linetypes = c(1,3,3,3)
	# Define x and y-axis text
	if(ebal){
		xtext = paste(xytext[1],':',data$model$name)
		ytext = paste(xytext[2],':',data$model$name)
	}else{
		vtext = bquote(.(tolower(varinfo$PlotName)) ~ ' (' ~.(varinfo$UnitsText) ~ ')')
		xtext = paste('Observed :',data$obs$name)
		ytext = paste('Modelled :',data$model$name)
	}
	
	# Prescribe data in scatterplots / regressions, removing qc flagged data if available:
	if((data$obs$qcexists) & (!ebal)){
		x_mod = data$obs$data[as.logical(data$obs$qc)]
		y_mod = data$model$data[as.logical(data$obs$qc)]
		if(data$bench$exist){
			b1_mod = data$bench[[ data$bench$index[1] ]]$data[as.logical(data$obs$qc)]
			if(data$bench$howmany == 2){
				b2_mod = data$bench[[ data$bench$index[2] ]]$data[as.logical(data$obs$qc)]
			}else if(data$bench$howmany == 3){
				b2_mod = data$bench[[ data$bench$index[2] ]]$data[as.logical(data$obs$qc)]
				b3_mod = data$bench[[ data$bench$index[3] ]]$data[as.logical(data$obs$qc)]
			}
		}
	}else{
		x_mod = data$obs$data
		y_mod = data$model$data
		if(data$bench$exist){
			b1_mod = data$bench[[ data$bench$index[1] ]]$data
			if(data$bench$howmany == 2){
				b2_mod = data$bench[[ data$bench$index[2] ]]$data
			}else if(data$bench$howmany == 3){
				b2_mod = data$bench[[ data$bench$index[2] ]]$data
				b3_mod = data$bench[[ data$bench$index[3] ]]$data
			}
		}
	}
	# Define plot boundaries:
	ymax=max(y_mod)
	ymin=min(y_mod)
	xmax=max(x_mod)
	xmin=min(x_mod)
	# First plot scatter of per timestep values:
	plot(x=x_mod,y=y_mod,main=bquote('Per time step' ~ .(vtext)),
		col=plotcols[1],xlab=xtext,ylab=ytext,
		type='p',pch='.',cex=3,ylim=c(min(ymin,xmin),max(ymax,xmax)),
		xlim=c(min(ymin,xmin),max(ymax,xmax)))
	# Overplot with better looking points:
	points(x=x_mod,y=y_mod,pch=20,col=plotcols[1],cex=0.35)
	# Define per time step regression coefficients:
	sline = lm(y_mod~x_mod,na.action=na.omit)
	if(data$bench$exist){
		b1line = lm(b1_mod~x_mod,na.action=na.omit)
		if(data$bench$howmany == 2){
			b2line = lm(b2_mod~x_mod,na.action=na.omit)
		}else if(data$bench$howmany == 3){
			b2line = lm(b2_mod~x_mod,na.action=na.omit)
			b3line = lm(b3_mod~x_mod,na.action=na.omit)
		}
	}
	# Add 1:1 line to plot:
	abline(a=0,b=1,col='black',lwd=1)
	# Add benchmark regresion lines to plot:
	if(data$bench$exist){
		abline(a=b1line$coefficients[1],b=b1line$coefficients[2],col=plotcols[2],lwd=4,lty=3)
		if(data$bench$howmany == 2){
			abline(a=b2line$coefficients[1],b=b2line$coefficients[2],col=plotcols[3],lwd=4,lty=3)
		}else if(data$bench$howmany == 3){
			abline(a=b2line$coefficients[1],b=b2line$coefficients[2],col=plotcols[3],lwd=4,lty=3)
			abline(a=b3line$coefficients[1],b=b3line$coefficients[2],col=plotcols[4],lwd=4,lty=3)
		}
	}
	# Add regresion line to plot
	abline(a=sline$coefficients[1],b=sline$coefficients[2],col=plotcols[1],lwd=5)
	# Add regression parameter text to plot:
	if(data$bench$exist){
		if(data$bench$howmany == 1){
			intdetail = paste('Intercept:',signif(sline$coefficients[1],2),',',signif(b1line$coefficients[1],2))
			graddetail = paste('Gradient:',signif(sline$coefficients[2],2),',',signif(b1line$coefficients[2],2))
		}else if(data$bench$howmany == 2){
			intdetail = paste('Intercept:',signif(sline$coefficients[1],2),',',
				signif(b1line$coefficients[1],2),',',signif(b2line$coefficients[1],2))
			graddetail = paste('Gradient:',signif(sline$coefficients[2],2),',',
				signif(b1line$coefficients[2],2),',',signif(b2line$coefficients[2],2))
		}else if(data$bench$howmany == 3){	
			intdetail = paste('Intercept:',signif(sline$coefficients[1],2),',',
				signif(b1line$coefficients[1],2),',',signif(b2line$coefficients[1],2),',',signif(b3line$coefficients[1],2))
			graddetail = paste('Gradient:',signif(sline$coefficients[2],2),',',
				signif(b1line$coefficients[2],2),',',signif(b2line$coefficients[2],2),',',signif(b3line$coefficients[2],2))
		}
	}else{
		intdetail = paste('Intercept:',signif(sline$coefficients[1],2))
		graddetail = paste('Gradient:',signif(sline$coefficients[2],2))
	}
	yrange = max(ymax,xmax) - min(ymin,xmin)
	text(x=min(ymin,xmin),y=max(ymax,xmax),labels=intdetail,pos=4)
	text(x=min(ymin,xmin),y=(min(ymin,xmin)+0.955*yrange),labels=graddetail,pos=4)
	if((data$obs$qcexists) & (!ebal)){
		text(x=(max(xmax,ymax)-yrange*0.45),y=max(ymax,xmax),labels='Gap-filled data removed',pos=4)
	}
	# Add legend to plot:
	legend(x=(max(xmax,ymax)-yrange*0.5),y=(max(ymax,xmax)-yrange*0.85),legendtext,
		lty=linetypes,col=plotcols,lwd=3,bty="n",yjust=0.8)
	# Define per time step metrics:
	if(data$bench$exist){
		if(data$bench$howmany == 1){
			metrics[[1]] = list(name='Grad',model_value=sline$coefficients[2],
				bench_value=list(bench1=b1line$coefficients[2]))	
			metrics[[2]] = list(name='Int',model_value=sline$coefficients[1],
				bench_value=list(bench1=b1line$coefficients[1]))
		}else if(data$bench$howmany == 2){
			metrics[[1]] = list(name='Grad',model_value=sline$coefficients[2],
				bench_value=list(bench1=b1line$coefficients[2],bench2=b2line$coefficients[2]))	
			metrics[[2]] = list(name='Int',model_value=sline$coefficients[1],
				bench_value=list(bench1=b1line$coefficients[1],bench2=b2line$coefficients[1]))
		}else if(data$bench$howmany == 3){
			metrics[[1]] = list(name='Grad',model_value=sline$coefficients[2],
				bench_value=list(bench1=b1line$coefficients[2],bench2=b2line$coefficients[2],
				bench3=b3line$coefficients[2]))	
			metrics[[2]] = list(name='Int',model_value=sline$coefficients[1],
				bench_value=list(bench1=b1line$coefficients[1],bench2=b2line$coefficients[1],
				bench3=b3line$coefficients[1]))
		}
	}else{
		metrics[[1]] = list(name='Grad',model_value=sline$coefficients[2])	
		metrics[[2]] = list(name='Int',model_value=sline$coefficients[1])
	}
	# If this an energy balance plot, add cumulative total:
	if(ebal){
		allebal = mean(data$obs$data-data$model$data)
		avebal = signif(mean(abs(data$obs$data-data$model$data)) , 3)
		alltext = paste('Total imbalance: ',signif(allebal,3),
			' W/m2 per timestep',sep='')
		avtext = paste('Mean deviation: ',avebal,' W/m2 per timestep',sep='')
		text(x=min(ymin,xmin),y=(min(ymin,xmin)+0.895*yrange),
			labels=alltext,pos=4)
		text(x=min(ymin,xmin),y=(min(ymin,xmin)+0.85*yrange),
			labels=avtext,pos=4)
	}
	######### Then plot scatter of daily averages #####################
	# Reshape data into clolumn time-steps-in-day, day rows:
	x_days=matrix(data$obs$data,ncol=tstepinday,byrow=TRUE)
	y_days=matrix(data$model$data,ncol=tstepinday,byrow=TRUE)
	xday = c()
	yday = c()
	if(data$bench$exist){
		b1_days=matrix(data$bench[[ data$bench$index[1] ]]$data,ncol=tstepinday,byrow=TRUE)
		b1day = c()
		if(data$bench$howmany == 2){
			b2_days=matrix(data$bench[[ data$bench$index[2] ]]$data,ncol=tstepinday,byrow=TRUE)
			b2day = c()
		}else if(data$bench$howmany == 3){
			b2_days=matrix(data$bench[[ data$bench$index[2] ]]$data,ncol=tstepinday,byrow=TRUE)
			b3_days=matrix(data$bench[[ data$bench$index[3] ]]$data,ncol=tstepinday,byrow=TRUE)
			b2day = c()
			b3day = c()
		}	
	}
	# Define daily mean time series, potentially removing qc time steps
	if((data$obs$qcexists) & (!ebal)){
		qc_days=matrix(as.logical(data$obs$qc),ncol=tstepinday,byrow=TRUE)
		for(i in 1:ndays){
			xday[i] = mean(x_days[i,qc_days[i,]])
			yday[i] = mean(y_days[i,qc_days[i,]])
		}
		if(data$bench$exist){
			if(data$bench$howmany == 1){
				for(i in 1:ndays){
					b1day[i] = mean(b1_days[i,qc_days[i,]])
				}
			}else if(data$bench$howmany == 2){
				for(i in 1:ndays){
					b1day[i] = mean(b1_days[i,qc_days[i,]])
					b2day[i] = mean(b2_days[i,qc_days[i,]])
				}
			}else if(data$bench$howmany == 3){
				for(i in 1:ndays){
					b1day[i] = mean(b1_days[i,qc_days[i,]])
					b2day[i] = mean(b2_days[i,qc_days[i,]])
					b3day[i] = mean(b3_days[i,qc_days[i,]])
				}
			}
		}	
	}else if(ebal){
    	ebalday = c()
    	for(i in 1:ndays){
        	xday[i] = mean(x_days[i,])
           	yday[i] = mean(y_days[i,])
            ebalday[i] = sum(x_days[i,]-y_days[i,])
        }
   	}else{ # not not an energy balance plot and QC data are missing
		for(i in 1:ndays){
			xday[i] = mean(x_days[i,])
			yday[i] = mean(y_days[i,])
		}
		if(data$bench$exist){
			if(data$bench$howmany == 1){
				for(i in 1:ndays){
					b1day[i] = mean(b1_days[i,])
				}
			}else if(data$bench$howmany == 2){
				for(i in 1:ndays){
					b1day[i] = mean(b1_days[i,])
					b2day[i] = mean(b2_days[i,])
				}
			}else if(data$bench$howmany == 3){
				for(i in 1:ndays){
					b1day[i] = mean(b1_days[i,])
					b2day[i] = mean(b2_days[i,])
					b3day[i] = mean(b3_days[i,])
				}
			}
		}
	}
	ymax=max(yday,na.rm=TRUE)
	ymin=min(yday,na.rm=TRUE)
	xmax=max(xday,na.rm=TRUE)
	xmin=min(xday,na.rm=TRUE)
	plot(x=xday,y=yday,main=bquote('Daily average' ~ .(vtext)),col=plotcols[1],
		xlab=xtext,ylab=ytext,type='p',pch='.',cex=3,
		ylim=c(min(ymin,xmin),max(ymax,xmax)),
		xlim=c(min(ymin,xmin),max(ymax,xmax)))
	# Overplot with better looking points:
	points(x=xday,y=yday,pch=20,col=plotcols[1],cex=0.35)
	# Define daily regression coefficients:
	sline = lm(yday~xday,na.action=na.omit)
	if(data$bench$exist){
		b1line = lm(b1day~xday,na.action=na.omit)
		if(data$bench$howmany == 2){
			b2line = lm(b2day~xday,na.action=na.omit)
		}else if(data$bench$howmany == 3){
			b2line = lm(b2day~xday,na.action=na.omit)
			b3line = lm(b3day~xday,na.action=na.omit)
		}
	}
	# Add 1:1 line:
	abline(a=0,b=1,col='black',lwd=1)
	# Add benchmark regresion lines to plot:
	if(data$bench$exist){
		abline(a=b1line$coefficients[1],b=b1line$coefficients[2],col=plotcols[2],lwd=4,lty=3)
		if(data$bench$howmany == 2){
			abline(a=b2line$coefficients[1],b=b2line$coefficients[2],col=plotcols[3],lwd=4,lty=3)
		}else if(data$bench$howmany == 3){
			abline(a=b2line$coefficients[1],b=b2line$coefficients[2],col=plotcols[3],lwd=4,lty=3)
			abline(a=b3line$coefficients[1],b=b3line$coefficients[2],col=plotcols[4],lwd=4,lty=3)
		}
	}
	# Add regresion line to plot
	abline(a=sline$coefficients[1],b=sline$coefficients[2],col=plotcols[1],lwd=5)
	# Add regression parameter text to plot:
	if(data$bench$exist){
		if(data$bench$howmany == 1){
			intdetail = paste('Intercept:',signif(sline$coefficients[1],2),',',signif(b1line$coefficients[1],2))
			graddetail = paste('Gradient:',signif(sline$coefficients[2],2),',',signif(b1line$coefficients[2],2))
		}else if(data$bench$howmany == 2){
			intdetail = paste('Intercept:',signif(sline$coefficients[1],2),',',
				signif(b1line$coefficients[1],2),',',signif(b2line$coefficients[1],2))
			graddetail = paste('Gradient:',signif(sline$coefficients[2],2),',',
				signif(b1line$coefficients[2],2),',',signif(b2line$coefficients[2],2))
		}else if(data$bench$howmany == 3){	
			intdetail = paste('Intercept:',signif(sline$coefficients[1],2),',',
				signif(b1line$coefficients[1],2),',',signif(b2line$coefficients[1],2),',',signif(b3line$coefficients[1],2))
			graddetail = paste('Gradient:',signif(sline$coefficients[2],2),',',
				signif(b1line$coefficients[2],2),',',signif(b2line$coefficients[2],2),',',signif(b3line$coefficients[2],2))
		}
	}else{
		intdetail = paste('Intercept:',signif(sline$coefficients[1],2))
		graddetail = paste('Gradient:',signif(sline$coefficients[2],2))
	}
	yrange = max(ymax,xmax) - min(ymin,xmin)
	text(x=min(ymin,xmin),y=max(ymax,xmax),labels=intdetail,pos=4)
	text(x=min(ymin,xmin),y=(min(ymin,xmin)+0.955*yrange),labels=graddetail,pos=4)
	# Add legend to plot:
	legend(x=(max(xmax,ymax)-yrange*0.5),y=(max(ymax,xmax)-yrange*0.85),legendtext,
		lty=linetypes,col=plotcols,lwd=3,bty="n",yjust=0.8)
	# If this an energy balance plot, add cumulative total:
	if(ebal){
		allebal = sum(ebalday)*timestepsize/3600/ndays # in Watt-hours
		avebal = signif(mean(abs(ebalday))*24/tstepinday, 3) # also in Watt-hours
		alltext = paste('Total imbalance: ',signif(allebal,3),
			' Wh/m2 per day',sep='')
		avtext = paste('Mean daily deviation: ',avebal,' Wh/m2',sep='')
		text(x=min(ymin,xmin),y=(min(ymin,xmin)+0.895*yrange),
			labels=alltext,pos=4)
		text(x=min(ymin,xmin),y=(min(ymin,xmin)+0.85*yrange),
			labels=avtext,pos=4)
	}else if(data$obs$qcexists){
		text(x=(max(xmax,ymax)-yrange*0.45),y=max(ymax,xmax),labels='Gap-filled data removed',pos=4)
	}
	# Record metrics:
	if(data$bench$exist){
		if(data$bench$howmany == 1){
			metrics[[3]] = list(name='DailyGrad',model_value=sline$coefficients[2],
				bench_value=list(bench1=b1line$coefficients[2]))	
			metrics[[4]] = list(name='DailyInt',model_value=sline$coefficients[1],
				bench_value=list(bench1=b1line$coefficients[1]))
		}else if(data$bench$howmany == 2){
			metrics[[3]] = list(name='DailyGrad',model_value=sline$coefficients[2],
				bench_value=list(bench1=b1line$coefficients[2],bench2=b2line$coefficients[2]))	
			metrics[[4]] = list(name='DailyInt',model_value=sline$coefficients[1],
				bench_value=list(bench1=b1line$coefficients[1],bench2=b2line$coefficients[1]))
		}else if(data$bench$howmany == 3){
			metrics[[3]] = list(name='DailyGrad',model_value=sline$coefficients[2],
				bench_value=list(bench1=b1line$coefficients[2],bench2=b2line$coefficients[2],
				bench3=b3line$coefficients[2]))	
			metrics[[4]] = list(name='DailyInt',model_value=sline$coefficients[1],
				bench_value=list(bench1=b1line$coefficients[1],bench2=b2line$coefficients[1],
				bench3=b3line$coefficients[1]))
		}
	}else{
		metrics[[3]] = list(name='DailyGrad',model_value=sline$coefficients[2])	
		metrics[[4]] = list(name='DailyInt',model_value=sline$coefficients[1])	
	}	

	result=list(err=TRUE,errtext=errtext, metrics=metrics)
	return(result)
} # End function PALSScatter
