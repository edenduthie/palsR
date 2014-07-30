# AnalysisEvapFrac.R
# Variable requirements: Qle and Qh
#
# This script produces a time series of the evaporative fraction
# [latent heat/(latent heat+sensible heat)] using a moving window
# of weekly averages of these two variables.
# 
# Gab Abramowitz CCRC, UNSW 2014 (palshelp at gmail dot com)
#
EvapFrac = function(obslabel,qledata,qhdata,ytext,legendtext,timestepsize,
	winsize=30,modlabel='no',qleqcdata=matrix(-1,nrow=1,ncol=1),
	qhqcdata=matrix(-1,nrow=1,ncol=1)){
	flux_threshold = 5 # W/m2
	ncurves = length(qledata[1,]) # Number of curves in final plot:
	ntsteps = length(qledata[,1]) # Number of timesteps in data:
	tstepinday=86400/obs_qle$timing$tstepsize # number of time steps in a day
	ndays = ntsteps/tstepinday # number of days in data set
	nyears=as.integer(ndays/365) # find # years in data set
	efrac = matrix(NA,(ndays-winsize-1),ncurves)
	# For each curve (i.e. model, obs etc):
	for(p in 1:ncurves){
		# Make sure we're only calcualting evap frac for +ve LE & H values:
		qlepos = pmax(flux_threshold,qledata[,p])
		qhpos = pmax(flux_threshold,qhdata[,p])
		# Reshape into column timesteps, row days:
		qle_days=matrix(qlepos,ncol=tstepinday,byrow=TRUE) 
		qh_days=matrix(qhpos,ncol=tstepinday,byrow=TRUE)
		for(i in 1:(ndays-winsize-1)){
			# Find evaporative fraction using averaging window:
			efrac[i,p] = mean(qle_days[i:(i+winsize-1),])/
				(mean(qh_days[i:(i+winsize-1),])+mean(qle_days[i:(i+winsize-1),]))
		}
	}
	xloc=c(1:(ndays-winsize-1)) # set location of x-coords in plot
	yaxmin=min(efrac) # y axis minimum in plot
	yaxmax=max(efrac) # y axis maximum in plot
	if(qleqcdata[1,1] != -1) {
		yaxmin = yaxmin - (yaxmax-yaxmin)*0.06
	}
	# Decide y axis range (add space for legend and score):
	yrange=c(yaxmin,yaxmax + 0.24*(yaxmax-yaxmin))
	plotcolours=LineColours() 
	# Plot observed time series:
	plot(xloc,efrac[,1],type="l",xaxt="n",xlab='Time',ylab=ytext,
		lwd=3,col=plotcolours[1],ylim=yrange,cex.lab=1.3,cex.axis=1.3)
	# Then plot other curves:
	if(ncurves>1){
		palsscore = c()
		for(p in 2:ncurves){ # for each additional curve
			lines(xloc,efrac[,p],lwd=3,col=plotcolours[p])
			# Define PALS score - normalised mean error:
			palsscore[p-1] = sum(abs(efrac[,1] - efrac[,p]))/sum(abs(mean(efrac[,1]) - efrac[,1]))
		}	
	}	
	# x-axis labels:
	xxat=c()
	xxlab=c()
	for(l in 1:nyears){
		xxat[(2*l-1)] = (l-1)*365 + 1
		xxat[(2*l)] = (l-1)*365 + 152	
		xxlab[(2*l-1)]=paste('1 Jan',obs_qle$syear+l-1)
		xxlab[(2*l)]=paste('1 Jun',obs_qle$syear+l-1)
	}	
	axis(1,at=xxat,labels=xxlab,cex.axis=1.3)
	if(modlabel=='no'){
		title(paste('Smoothed evaporative fraction:  Obs - ',
			obslabel,sep=''),cex.main=1.2) # add title
	}else{
		title(paste('Smoothed evaporative fraction:  Obs - ',obslabel,
			'  Mod - ',modlabel,sep=''),cex.main=1.2) # add title
	}		
	legend(-1,yrange[2],legendtext[1:ncurves],lty=1,col=plotcolours[1:ncurves],
		lwd=3,bty="n",yjust=0.85)
	if(ncurves>1){
		scorestring = paste(signif(palsscore,digits=3),collapse=', ')
		scoretext = paste('Score: ',scorestring,'\n','(NME)',sep='')
		text((0.75*xloc[length(xloc)]),yrange[2],scoretext,pos=1)
	}
	# Calculate QC time series information, if it exists:
		if(qleqcdata[1,1] != -1){
			qcliney1 = yaxmin # y-location of qc line
			qcliney2 = yaxmin - (yaxmax-yaxmin)*0.03# y-location of qc line
			qctexty1 = yaxmin + (yaxmax-yaxmin)*0.04 # y-location of qc text
			qctexty2 = yaxmin - (yaxmax-yaxmin)*0.02 # location of H and E in front of lines
			qcpcqle = signif((1-mean(qleqcdata[,1]))*100,2) # % of data that's gapfilled
			qcpcqh = signif((1-mean(qhqcdata[,1]))*100,2) # % of data that's gapfilled
			# Construct line-plottable version of qc timeseries:
			origline_qle =  qcliney1/(qleqcdata[,1]) # 0s will become 'Inf'
			gapline_qle = (qcliney1/(qleqcdata[,1]-1))*-1 # 1s will become 'Inf'
			origline_qh =  qcliney2/(qhqcdata[,1]) # 0s will become 'Inf'
			gapline_qh = (qcliney2/(qhqcdata[,1]-1))*-1 # 1s will become 'Inf'
			# Plot qc time series line:
			xloc_qc = c(1:length(origline_qle))/length(origline_qle) * length(xloc)
			lines(xloc_qc,origline_qle,lwd=6,col='gray80')
			lines(xloc_qc,gapline_qle,lwd=3,col='indianred')
			lines(xloc_qc,origline_qh,lwd=6,col='gray80')
			lines(xloc_qc,gapline_qh,lwd=3,col='indianred')
			text(x=1,y=qctexty1,cex=0.9,pos=4,
				labels=paste(qcpcqle,'% of observed E and ',qcpcqh,
				'% observed H are gap-filled:',sep=''))
			text(x=1,y=qctexty2,cex=0.8,pos=2,
				labels=paste('E\nH',sep=''))
		}
	
} # end function EvapFrac
