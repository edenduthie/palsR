# AnalysisPdf.R
#
# Plots probability density functions
#
# Gab Abramowitz UNSW 2014 (palshelp at gmail dot com)

PALSPdf = function(obslabel,pdfdata,varname,xtext,legendtext,timing,
	nbins=500,modlabel='no',vqcdata=matrix(-1,nrow=1,ncol=1)){
	errtext = 'ok'
	metrics = list()
	xcut = 1/100
	ncurves = length(pdfdata[1,]) # Number of curves in final plot:
	ntsteps = length(pdfdata[,1]) # Number of timesteps in data:
	tstepinday=86400/timing$tstepsize # number of time steps in a day
	ndays = ntsteps/tstepinday # number of days in data set
	plotcolours=LineColours() 
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
		if(ncurves==2){ # model only
			metrics[[1]] = list(name='%Overlap',model_value=overl[1])	
		}else if(ncurves==3){
			metrics[[1]] = list(name='%Overlap',model_value=overl[1],bench_value=list(bench1=overl[2]))	
		}else if(ncurves==4){
			metrics[[1]] = list(name='%Overlap',model_value=overl[1],
				bench_value=list(bench1=overl[2],bench2=overl[3]))
		}else if(ncurves==5){
			metrics[[1]] = list(name='%Overlap',model_value=overl[1],
				bench_value=list(bench1=overl[2],bench2=overl[3],bench3=overl[4]))
		}
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
		legend(xlow+(xhigh-xlow)*0.75,ymax,legend=ltext,lty=1,
			col=lcols,lwd=llwd,bty="n")
	}else{
		legend(xlow+(xhigh-xlow)*0.75,ymax,legend=legendtext[1:ncurves],lty=1,
			col=plotcolours[1:ncurves],lwd=3,bty="n")
	}
	if(modlabel=='no'){
		title(paste(varname[1],' density:  Obs - ',obslabel,sep=''))
	}else{
		title(paste(varname[1],' density:   Obs - ',obslabel,'   Model - ',modlabel,sep=''))
	}
	result=list(err=FALSE,errtext=errtext,metrics=metrics)
	return(result)
}
