# AnalysisScatter.R
#
# Plots a scatterplot of model vs obs
#
# Gab Abramowitz UNSW 2014 (palshelp at gmail dot com)
#
PALSScatter = function(obslabel,y_data,x_data,varname,vtext,
	xytext,timestepsize,whole,ebal=FALSE,modlabel='no',
	vqcdata=matrix(-1,nrow=1,ncol=1)){
	#
	errtext = 'ok'
	ntsteps = length(x_data) # Number of timesteps in data
	tstepinday=86400/timestepsize # number of time steps in a day
	ndays = ntsteps/tstepinday # number of days in data set
	# Plot layout:
	par(mfcol=c(1,2),mar=c(4,4,3,0.5),oma=c(0,0,0,1),
		mgp=c(2.5,0.7,0),ps=12,tcl=-0.4)
	if(modlabel=='no'){
		xtext = paste(xytext[1],':',obslabel)
		ytext = paste(xytext[2],':',obslabel)
	}else if(ebal & (modlabel != 'no')){
		xtext = paste(xytext[1],':',modlabel)
		ytext = paste(xytext[2],':',modlabel)
	}else{
		xtext = paste(xytext[1],':',obslabel)
		ytext = paste(xytext[2],':',modlabel)
	}
	if((vqcdata[1,1] != -1) & (!ebal)){
		x_mod = x_data[as.logical(vqcdata[,1])]
		y_mod = y_data[as.logical(vqcdata[,1])]
	}else{
		x_mod = x_data
		y_mod = y_data
	}
	ymax=max(y_mod)
	ymin=min(y_mod)
	xmax=max(x_mod)
	xmin=min(x_mod)
	# First plot scatter of per timestep values:
	plot(x=x_mod,y=y_mod,main=bquote('Per time step' ~ .(vtext)),
		col='darkblue',xlab=xtext,ylab=ytext,
		type='p',pch='.',cex=3,ylim=c(min(ymin,xmin),max(ymax,xmax)),
		xlim=c(min(ymin,xmin),max(ymax,xmax)))
	sline = lm(y_mod~x_mod)
	# Add 1:1 line:
	abline(a=0,b=1,col='black',lwd=1)
	# Add regresion line to plot
	abline(a=sline$coefficients[1],b=sline$coefficients[2],col='darkblue',lwd=4)
	# Add regression parameter text to plot:
	intdetail = paste('Intercept:',signif(sline$coefficients[1],3))
	graddetail = paste('Gradient:',signif(sline$coefficients[2],3))
	yrange = max(ymax,xmax) - min(ymin,xmin)
	text(x=min(ymin,xmin),y=max(ymax,xmax),labels=intdetail,pos=4)
	text(x=min(ymin,xmin),y=(min(ymin,xmin)+0.955*yrange),labels=graddetail,pos=4)
	if((vqcdata[1,1] != -1) & (!ebal)){
		text(x=(max(xmax,ymax)-yrange*0.5),y=max(ymax,xmax),labels='Gap-filled data removed',pos=4)
	}
	# If this an energy balance plot, add cumulative total:
	if(ebal){
		allebal = mean(x_data-y_data)
		avebal = signif(mean(abs(x_data-y_data)) , 3)
		alltext = paste('Total imbalance: ',signif(allebal,3),
			' W/m2 per timestep',sep='')
		avtext = paste('Mean deviation: ',avebal,' W/m2 per timestep',sep='')
		text(x=min(ymin,xmin),y=(min(ymin,xmin)+0.895*yrange),
			labels=alltext,pos=4)
		text(x=min(ymin,xmin),y=(min(ymin,xmin)+0.85*yrange),
			labels=avtext,pos=4)
	}
	# Then plot scatter of daily averages:
	# Reshape data into clolumn time-steps-in-day, day rows:
	x_days=matrix(x_data,ncol=tstepinday,byrow=TRUE)
	y_days=matrix(y_data,ncol=tstepinday,byrow=TRUE)
	xday = c()
	yday = c()
	if((vqcdata[1,1] != -1) & (!ebal)){
		qc_days=matrix(as.logical(vqcdata[,1]),ncol=tstepinday,byrow=TRUE)
		for(i in 1:ndays){
			xday[i] = mean(x_days[i,qc_days[i,]])
			yday[i] = mean(y_days[i,qc_days[i,]])
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
	}
	ymax=max(yday,na.rm=TRUE)
	ymin=min(yday,na.rm=TRUE)
	xmax=max(xday,na.rm=TRUE)
	xmin=min(xday,na.rm=TRUE)
	plot(x=xday,y=yday,main=bquote('Daily average' ~ .(vtext)),col='darkblue',
		xlab=xtext,ylab=ytext,type='p',pch='.',cex=3,
		ylim=c(min(ymin,xmin),max(ymax,xmax)),
		xlim=c(min(ymin,xmin),max(ymax,xmax)))
	sline = lm(yday~xday)
	# Add 1:1 line:
	abline(a=0,b=1,col='black',lwd=1)
	# Add regresion line to plot
	abline(a=sline$coefficients[1],b=sline$coefficients[2],col='darkblue',lwd=4)
	# Add regression parameter text to plot:
	intdetail = paste('Intercept:',signif(sline$coefficients[1],3))
	graddetail = paste('Gradient:',signif(sline$coefficients[2],3))
	yrange = max(ymax,xmax) - min(ymin,xmin)
	text(x=min(ymin,xmin),y=max(ymax,xmax),labels=intdetail,pos=4)
	text(x=min(ymin,xmin),y=(min(ymin,xmin)+0.955*yrange),labels=graddetail,pos=4)
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
	}else if(vqcdata[1,1] != -1){
		text(x=(max(xmax,ymax)-yrange*0.5),y=max(ymax,xmax),labels='Gap-filled data removed',pos=4)
	}
	result=list(errtext=errtext)
	return(result)
} # End function PALSScatter
