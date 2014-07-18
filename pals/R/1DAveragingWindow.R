# AnalysisAveragingWindow.R
#
# Function for plotting linear regression gradient, rmse, model
# and observed correlation and standard deviation for
# a temporally averaged model output flux
#
# Gab Abramowitz CCRC, UNSW 2014 (palshelp at gmail dot com)
#
AveragingWindow	= function(obslabel,modlabel,mod_data,obs_data,
	varname,ytext,timestepsize){
	library(boot) # load bootstrap library
	errtext = 'ok'
	metrics = list()
	windowstepsize=2 # resolution of graph
	maxwindow = 30 # in days, the largest averaging window
	# Plot layout:
	par(mfcol=c(2,2),mar=c(4,4,3,0.5),oma=c(0,0,0,1),
		mgp=c(2.5,0.7,0),ps=16,tcl=-0.4)
	tstepinday=86400/timestepsize # number of time steps in a day
	numcalcs=maxwindow*tstepinday/windowstepsize
	tsteps=length(mod_data) # total number of timesteps
	mvals = c() # initialise gradient values
	mwidth =c() # initialise 95% confidence interval for regression
	rvals = c() # initialise correlation coefficient values
	rmse = c()  # intialise RMSE
	stdevi = matrix(0,2,numcalcs)
	xloc = c() # initialise x ticks
	for(i in 1:numcalcs){
		# Calculate window size:
		windowsize=windowstepsize*i
		# Reduce data set size if necessary for reshaping:
		numdump=tsteps %% windowsize # "%%" is modulo
		nwindows=(tsteps-numdump)/windowsize # number of windows
		sqerr=c()
		flav=matrix(0,nwindows,2) # init
		# Reshape data:
		mdat=matrix(mod_data[1:(tsteps-numdump)],windowsize,nwindows)
		odat=matrix(obs_data[1:(tsteps-numdump)],windowsize,nwindows)
		for(j in 1:nwindows){ # calculate average flux for each window
			flav[j,1]=mean(mdat[,j])	# vector of averages
			flav[j,2]=mean(odat[,j])	# vector of averages
			sqerr[j]=(flav[j,1]-flav[j,2])^2
		}
		# Perform least squares regression (b/w model ad obs):
		rgrs=lsfit(flav[,2],flav[,1])
		mvals[i]=rgrs$coef[[2]] # store gradient values for plot
		# Get 95% confidence interval:
		linval=c() # init
		lindev=c() # init
		moddev=c() # init
		meanmod=mean(flav[j,1])
		for(k in 1:nwindows){
			linval[k]=rgrs$coef[[2]]*flav[k,1]+rgrs$coef[[1]]
			lindev[k]=(linval[k]-flav[k,2])^2
			moddev[k]=(flav[k,1]-meanmod)^2
		}
		# 95% confidence interval:
		mwidth[i]=sqrt(sum(lindev)/nwindows)*1.96/sqrt(sum(moddev))
		# Get correlation cofficient:
		rvals[i]=corr(flav)
		# Get variance:
		stdevi[1,i] = sd(flav[,1]) # model variance
		stdevi[2,i] = sd(flav[,2]) # obs variance
		# Calculate RMSE:
		rmse[i]=sqrt(mean(sqerr))	
		rm(mdat,odat,flav,sqerr) # clear variables which will have different lengths
		xloc[i]=windowsize/tstepinday
	}
	alltitle=paste('Obs:',obslabel,'  Model:',modlabel)
	# Draw RMSE plot:
	plot(xloc,rmse,type="l",xaxt="n",xlab='Averaging window size (days)',
		ylab=paste('RMSE of',varname[1]),lwd=3,col='black')
	# Work out x-axis ticks:
	tix=c(0,as.integer(maxwindow/4),as.integer(2*maxwindow/4),
	as.integer(3*maxwindow/4),maxwindow)
	axis(1,at=tix,labels=as.character(tix))
	title(alltitle) # add title
	# Draw gradient plot:
	plot(xloc,mvals,type="l",xaxt="n",xlab='Averaging window size (days)',
		ylab=paste(varname[1],'Mod v obs gradient'),lwd=1,col='blue',
		ylim=c(min(mvals-mwidth),max(mvals+mwidth)))
	# Sort out confidence polygon:
	polyX=c(xloc,xloc[numcalcs:1])
	polyY=c(mvals+mwidth,mvals[numcalcs:1]-mwidth[numcalcs:1])
	polygon(polyX,polyY,col='grey')
	lines(xloc,mvals,lwd=3,col='blue')		
	axis(1,at=tix,labels=as.character(tix))
	title(alltitle) # add title
	# Draw correlation plot:
	plot(xloc,rvals,type="l",xaxt="n",xlab='Averaging window size (days)',
		ylab=paste('Mod, obs CorrCoef',varname[1]),lwd=3,col='black')
	axis(1,at=tix,labels=as.character(tix))
	title(alltitle) # add title
	# Draw std dev plot:
	plot(xloc,stdevi[1,],type="l",xaxt="n",
		xlab='Averaging window size (days)',
		ylab=paste('Standard deviation of',varname[1]),lwd=3,col='blue',
		ylim=c(min(stdevi[1,],stdevi[2,]),
		max(stdevi[1,],stdevi[2,])))
	lines(xloc,stdevi[2,],lwd=3,col='black')
	axis(1,at=tix,labels=as.character(tix))
	title(alltitle) # add title
	ypos = min(stdevi[1,],stdevi[2,]) +
		0.8*(max(stdevi[1,],stdevi[2,])-min(stdevi[1,],stdevi[2,]))
	legend(as.integer(3*maxwindow/4),ypos,c('Model','Obs'),lty=1,
		col=c('blue','black'),lwd=3,bty="n")
	metrics[[1]] = list(name='NoMetric',model_value=NA)
	result=list(err=FALSE,errtext=errtext,metrics=metrics)
	return(result)
} # End function avwinflux
