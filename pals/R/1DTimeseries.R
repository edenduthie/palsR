# AnalysisTimeseries.R
#
# Plots simple or smoothed timeseries of a variable
#
# Gab Abramowitz UNSW 2014 (palshelp at gmail dot com)

Timeseries = function(obslabel,tsdata,varname,ytext,legendtext,
	plotcex,timing,smoothed=FALSE,winsize=1,plotcolours,modlabel='no',
	vqcdata=matrix(-1,nrow=1,ncol=1)){
	#
	errtext = 'ok'
	metrics = list()
	ncurves = length(tsdata[1,]) # Number of curves in final plot:
	ntsteps = length(tsdata[,1]) # Number of timesteps in data:
	tstepinday=86400/timing$tstepsize # number of time steps in a day
	ndays = ntsteps/tstepinday # number of days in data set
	nyears=as.integer(ndays/365) # find # years in data set
	# x-axis labels:
	xxat=c()
	xxlab=c()
	data_smooth = matrix(NA,(ndays-winsize-1),ncurves) # init
	if(smoothed){
		for(p in 1:ncurves){
			# Reshape into column timesteps, row days:
			data_days=matrix(tsdata[,p],ncol=tstepinday,byrow=TRUE) 
			for(i in 1:(ndays-winsize-1)){
				# Find evaporative fraction using averaging window:
				data_smooth[i,p] = mean(data_days[i:(i+winsize-1),])
			}
			if(p==1){
				yvalmin = as.character(signif(min(tsdata[,p]),3))
				yvalmax = as.character(signif(max(tsdata[,p]),3))
				datamean = as.character(signif(mean(tsdata[,p]),3))
				datasd = as.character(signif(sd(tsdata[,p]),3))

			}else{
				yvalmin = paste(yvalmin,', ',as.character(signif(min(tsdata[,p]),3)),sep='')
				yvalmax = paste(yvalmax,', ',as.character(signif(max(tsdata[,p]),3)),sep='')
				datamean = paste(datamean,', ',as.character(signif(mean(tsdata[,p]),3)),sep='')
				datasd = paste(datasd,', ',as.character(signif(sd(tsdata[,p]),3)),sep='')
			}
		}
		ymin = signif(min(data_smooth),3)
		ymax = signif(max(data_smooth),3)
		# If we're adding a gap-filling QC line, make space for it:
		if(vqcdata[1,1] != -1) {
			ymin = ymin - (ymax-ymin)*0.06
		}
		xmin = 1
		xmax = length(data_smooth[,1])
		xloc=c(1:xmax)
		# Draw plot:
		plot(xloc,data_smooth[,1],type="l",ylab=ytext,lwd=3,
			col=plotcolours[1],ylim=c((ymin),(ymin + (ymax-ymin)*1.2)),
			xaxt='n',cex.lab=plotcex,cex.axis=plotcex,xlab='',mgp = c(2.5,1,0))
		# Calculate NME scores:
		if(ncurves>1){
			smoothscore = c()
			allscore = c()
			for(p in 2:ncurves){ # for each additional curve
				lines(data_smooth[,p],lwd=3,col=plotcolours[p])
				smoothscore[p-1] = sum(abs(data_smooth[,1] - data_smooth[,p]))/
					sum(abs(mean(data_smooth[,1]) - data_smooth[,1]))
				allscore[p-1] = sum(abs(tsdata[,1] - tsdata[,p]))/
					sum(abs(mean(tsdata[,1]) - tsdata[,1]))
			}
			# Report NME metric:
			metricname = paste('NME',winsize,'day',sep='')
			if(ncurves==2){ # model only
				metrics[[1]] = list(name='Bias',model_value=mean(tsdata[,2]-tsdata[,1],na.rm=TRUE))	
				metrics[[2]] = list(name='NME',model_value=allscore[1])	
				metrics[[3]] = list(name=metricname,model_value=smoothscore[1])	
			}else if(ncurves==3){
				metrics[[1]] = list(name='Bias',model_value=mean(tsdata[,2]-tsdata[,1],na.rm=TRUE),
					bench_value=list(bench1=mean(tsdata[,3]-tsdata[,1],na.rm=TRUE) ))
				metrics[[2]] = list(name='NME',model_value=allscore[1],bench_value=list(bench1=allscore[2]))	
				metrics[[3]] = list(name=metricname,model_value=smoothscore[1],
					bench_value=list(bench1=smoothscore[2]))	
			}else if(ncurves==4){
				metrics[[1]] = list(name='Bias',model_value=mean(tsdata[,2]-tsdata[,1],na.rm=TRUE),
					bench_value=list(bench1=mean(tsdata[,3]-tsdata[,1],na.rm=TRUE),
					bench2=mean(tsdata[,4]-tsdata[,1],na.rm=TRUE) ))
				metrics[[2]] = list(name='NME',model_value=allscore[1],
					bench_value=list(bench1=allscore[2],bench2=allscore[3]))
				metrics[[3]] = list(name=metricname,model_value=smoothscore[1],
					bench_value=list(bench1=smoothscore[2],bench2=smoothscore[3]))	
			}else if(ncurves==5){
				metrics[[1]] = list(name='Bias',model_value=mean(tsdata[,2]-tsdata[,1],na.rm=TRUE),
					bench_value=list(bench1=mean(tsdata[,3]-tsdata[,1],na.rm=TRUE),
					bench2=mean(tsdata[,4]-tsdata[,1],na.rm=TRUE),
					bench3=mean(tsdata[,5]-tsdata[,1],na.rm=TRUE) ))
				metrics[[2]] = list(name='NME',model_value=allscore[1],
					bench_value=list(bench1=allscore[2],bench2=allscore[3],bench3=allscore[4]))
				metrics[[3]] = list(name=metricname,model_value=smoothscore[1],
					bench_value=list(bench1=smoothscore[2],bench2=smoothscore[3],bench3=smoothscore[4]))	
			}
		}
		for(l in 1:nyears){
			xxat[(2*l-1)] = (l-1)*365 + 1
			xxat[(2*l)] = (l-1)*365 + 152	
			xxlab[(2*l-1)]=paste('1 Jan',substr(as.character(timing$syear+l-1),3,4))
			xxlab[(2*l)]=paste('1 Jun',substr(as.character(timing$syear+l-1),3,4))
		}
		# place legend:
		legend(xmin-(xmax-xmin)*0.03,(ymin + (ymax-ymin)*1.24),legend=legendtext[1:ncurves],lty=1,
			col=plotcolours[1:ncurves],lwd=3,bty="n",cex=max((plotcex*0.75),1))
		# Add title:
		if(modlabel=='no'){
			title(paste('Smoothed ',varname[1],': ',winsize,'-day running mean.   Obs - ',
				obslabel,sep=''),cex.main=plotcex)
		}else{
			title(paste('Smoothed ',varname[1],': ',winsize,'-day running mean.   Obs - ',
				obslabel,'   Model - ',modlabel,sep=''),cex.main=plotcex)
		}
		# Add Max/Min/Mean/SD numbers:
		text(x=(xmin+(xmax-xmin)*0.25),y=c(ymin + (ymax-ymin)*1.19,ymin + (ymax-ymin)*1.14),
			labels=c(paste('Min = (',yvalmin,')',sep=''),
			paste('Max = (',yvalmax,')',sep='')),
			cex=max((plotcex*0.75),1),pos=4)
		text(x=(xmin+(xmax-xmin)*0.25),y=c(ymin + (ymax-ymin)*1.09,ymin + (ymax-ymin)*1.04),
			labels=c(paste('Mean = (',datamean,')',sep=''),paste('SD = (',datasd,')',sep='')),
			cex=max((plotcex*0.75),1),pos=4)
		# Add NME scores to plot (if there is at least one model output):
		if(ncurves>1){
			sscorestring = paste(signif(smoothscore,digits=3),collapse=', ')
			ascorestring = paste(signif(allscore,digits=3),collapse=', ')
			text(x=c(xmin+(xmax-xmin)*0.65),y=(ymin + (ymax-ymin)*1.18),
				labels=paste('Score_smooth: ',sscorestring,sep=''),,pos=4)
			text(x=c(xmin+(xmax-xmin)*0.65),y=(ymin + (ymax-ymin)*1.12),
				labels= paste('Score_all: ',ascorestring,sep=''),pos=4)
			text(x=c(xmin+(xmax-xmin)*0.65),y=(ymin + (ymax-ymin)*1.06),
				labels=' (NME)',pos=4)
		}
		# Calculate QC time series information, if it exists:
		if(vqcdata[1,1] != -1){
			qcliney = ymin - (ymax-ymin)*0.015# y-location of qc line
			qctexty = ymin + (ymax-ymin)*0.02 # y-location of qc text
			qcpc = signif((1-mean(vqcdata[,1]))*100,2) # % of data that's gapfilled
			# Construct line-plottable version of qc timeseries:
			origline =  qcliney/(vqcdata[,1]) # 0s will become 'Inf'
			gapline = (qcliney/(vqcdata[,1]-1))*-1 # 1s will become 'Inf'
			# Plot qc time series line:
			xloc_qc = c(1:length(origline))/length(origline) * length(xloc)
			lines(xloc_qc,origline,lwd=6,col='gray80')
			lines(xloc_qc,gapline,lwd=3,col='indianred')
			text(x=xmin,y=qctexty,cex=max((plotcex*0.75),0.85),pos=4,
				labels=paste(qcpc,'% of observed ',varname[1],' is gap-filled:',sep=''))
		}		
	}else{
		# this code not functioning but kept for future modification:
		yvalmin = signif(min(tsdata),3)
		yvalmax = signif(max(tsdata),3)
		datamean = signif(mean(tsdata[,1]),3)
		datasd = signif(sd(tsdata[,1]),3)
		ymin = yvalmin
		ymax = yvalmax
		xmin = 1
		xmax = ntsteps
		xloc=c(1:xmax)
		plot(xloc,tsdata[,1],type="l",ylab=ytext,lwd=3,
			col=plotcolours[1],ylim=c(ymin,(ymin + (ymax-ymin)*1.3)),
			xaxt='n',cex.lab=plotcex,cex.axis=plotcex,xlab='')
		# Add smoothed curve over whole timeseries:
		data_days=matrix(tsdata[,1],ncol=tstepinday,byrow=TRUE) 
		data_smooth = c()
		dayssmooth = 30
		for(i in 1:(ndays-dayssmooth-1)){
			# Find evaporative fraction using averaging window:
			data_smooth[i] = mean(data_days[i:(i+dayssmooth-1),])
		}
		xct = c(1:(ndays-dayssmooth-1))
		xsmooth = xct*tstepinday + (tstepinday*dayssmooth / 2 - tstepinday)
		lines(xsmooth,data_smooth,lwd=3,col='gray')
		
		if(ncurves>1){
			for(p in 2:ncurves){ # for each additional curve
				lines(tsdata[,p],lwd=3,col=plotcolours[p])
			}	
		}
		for(l in 1:nyears){
			xxat[(2*l-1)] = (l-1)*365*tstepinday + 1
			xxat[(2*l)] = (l-1)*365*tstepinday + 183*tstepinday
			xxlab[(2*l-1)]=paste('1 Jan',substr(as.character(timing$syear+l-1),3,4))
			xxlab[(2*l)]=paste('1 Jul',substr(as.character(timing$syear+l-1),3,4))
		}
		legend(0-(xmax-xmin)*0.05,(ymin + (ymax-ymin)*1.42),legend=legendtext[1:ncurves],lty=1,
			col=plotcolours[1:ncurves],lwd=3,bty="n",cex=max((plotcex*0.75),1))
		title(paste(obslabel,varname[1]),cex.main=plotcex)
		# Locations of max,min,mean,sd text:
		stattextx = c(xmin,xmin+(xmax-xmin)*0.5)
		stattexty = c(ymin + (ymax-ymin)*1.18,ymin + (ymax-ymin)*1.24)
		# Write max,min,mean,sd to plot in two lines:
		text(x=stattextx,y=stattexty[2],
			labels=c(paste('Min = ',ymin,sep=''),paste('Max = ',ymax,sep='')),
			cex=max((plotcex*0.75),1),pos=4)
		text(x=stattextx,y=stattexty[1],
			labels=c(paste('Mean = ',datamean,sep=''),paste('SD = ',datasd,sep='')),
			cex=max((plotcex*0.75),1),pos=4)
		# Calculate QC time series information, if it exists:
		if(vqcdata[1,1] != -1){
			qcliney = ymin + (ymax-ymin)*1.04 # y-location of qc line
			qctexty = ymin + (ymax-ymin)*1.09 # y-location of qc text
			qcpc = signif((1-mean(vqcdata[,1]))*100,2) # % of data that's gapfilled
			# Construct line-plottable version of qc timeseries:
			origline =  qcliney/(vqcdata[,1]) # 0s will become 'Inf'
			gapline = (qcliney/(vqcdata[,1]-1))*-1 # 1s will become 'Inf'
			# Plot qc time series line:
			lines(origline,lwd=5,col='gray80')
			lines(gapline,lwd=2,col='red')
			text(x=stattextx[1],y=qctexty,cex=max((plotcex*0.75),1),pos=4,
				labels=paste(qcpc,'% of time series is gap-filled:',sep=''))
		}
	}
	axis(1,at=xxat,labels=xxlab,cex.axis=plotcex)
	result = list(err=FALSE,errtext = errtext,metrics=metrics)
	return(result)
}