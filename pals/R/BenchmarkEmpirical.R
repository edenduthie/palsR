# BenchmarkEmpirical.R
#
# This set of functions is used for training empirical models used to benchmark
# LSM simulations as well as using these trained models to perform benchmark 
# simulations.
#
# Gab Abramowitz CCRC, UNSW 2012 (palshelp at gmail dot com)
#

GenerateBenchmarkSet = function(TrainPathsMet,TrainPathsFlux,
	PredictPathMet,DatSetName,DataSetVersion,PredictNcPath,SiteLat,
	SiteLon,varnames,varsunits,removeflagged){
	
	library(ncdf) # load netcdf library
	
	ntrainsites = length(TrainPathsMet) # number of training sites
	
	# Define netcdf file for storing benchmark simulations:
	missing_value=NcMissingVal # default missing value for all variables
	# Define x, y dimensions
	xd = dim.def.ncdf('x',vals=c(1),units='')	
	yd = dim.def.ncdf('y',vals=c(1),units='')
	# Open met prediction file to copy timing details:
	mfid=open.ncdf(PredictPathMet,readunlim=FALSE)
	timeunits=att.get.ncdf(mfid,'time','units')
	timedata=get.var.ncdf(mfid,'time')
	close.ncdf(mfid)
	# Define time dimension:
	td = dim.def.ncdf('time',unlim=TRUE,units=timeunits$value,vals=timedata)
	# Begin defining variables (time independent first):
	benchvars = list() # initialise
	# Define latitude variable:
	benchvars[[1]] = var.def.ncdf('latitude',units='degrees_north',
		dim=list(xd,yd),missval=missing_value,longname='Latitude')
	# Define longitude variable:
	benchvars[[2]] = var.def.ncdf('longitude',units='degrees_east',
		dim=list(xd,yd),missval=missing_value,longname='Longitude')
	# Define time varying benchmark variables:
	vctr = 3 # variable counter
	for(v in 1:length(varnames)){
		for(b in 1:length(benchnames)){
			benchvars[[vctr]]=var.def.ncdf(name=paste(varnames[v],'_',
				benchnames[b],sep=''),units=varsunits[v],dim=list(xd,yd,td),
				missval=missing_value,
				longname=paste('Empirical',varnames[v],
					'benchmark timeseries',benchnames[b]))
			vctr = vctr + 1
		}
	}
	# If Qle, Qh and Qg empirical benchmarks are being generated, define Rnet as well:
	if(any(varnames=='Qle') & any(varnames=='Qh') & any(varnames=='Qg')){
		for(b in 1:length(benchnames)){
			benchvars[[vctr]]=var.def.ncdf(name=paste('Rnet_',
				benchnames[b],sep=''),units=RnetUnitsName[1],dim=list(xd,yd,td),
				missval=missing_value,
				longname=paste('Empirical Rnet benchmark timeseries',benchnames[b]))
			vctr = vctr + 1
		}	
	}
		
	# Create benchmark netcdf file:
	ncid = create.ncdf(filename=PredictNcPath,vars=benchvars)
	# Add ancillary details about benchmarks:
	for(b in 1:length(benchnames)){
		# First just create string of input variable names:
		benchin = ''
		cnt = 1
		repeat{
			benchin = paste(benchin,benchx[[b]][cnt],sep='')
			if(length(benchx[[b]]) == cnt){ 
				break 
			}else{
				benchin = paste(benchin,', ',sep='')
			}
			cnt = cnt + 1
		}
		for(v in 1:length(varnames)){
			# Save this string as benchmark_inputs attribute:
			att.put.ncdf(ncid,paste(varnames[v],'_',benchnames[b],sep=''),
				'benchmark_inputs',benchin)
			# Add empirical model type attribute:
			att.put.ncdf(ncid,paste(varnames[v],'_',benchnames[b],sep=''),
				'benchmark_modeltype',benchtype[b])
		}
		# If we're creating Rnet as well, add attributes:
		if(any(varnames=='Qle') & any(varnames=='Qh') & any(varnames=='Qg')){
			att.put.ncdf(ncid,paste('Rnet_',benchnames[b],sep=''),
				'source',paste('Constructed as a sum of',
				benchnames[b],' Qle, Qh and Qg'))
		}
	}
	# Write global attributes:
	att.put.ncdf(ncid,varid=0,attname='Production_time',
		attval=as.character(Sys.time()))
	att.put.ncdf(ncid,varid=0,attname='Production_source',
		attval='PALS empirical benchmark simulations')
	if(removeflagged){ # Note if gap-filled data has been excluded 
		att.put.ncdf(ncid,varid=0,attname='Exclusions',
 			attval='Empirical models trained only on non-gap-filled data')
	}
	att.put.ncdf(ncid,varid=0,attname='PALS_dataset_name',
		attval=DataSetName)
	att.put.ncdf(ncid,varid=0,attname='PALS_dataset_version',
		attval=DataSetVersion)
	att.put.ncdf(ncid,varid=0,attname='Contact',
		attval='palshelp@gmail.com')
	# Add time-independent variable data to file:
	put.var.ncdf(ncid,'latitude',vals=SiteLat)
	put.var.ncdf(ncid,'longitude',vals=SiteLon)
	# Create paths to training data files:
	TrainPaths = c(TrainPathsMet,TrainPathsFlux)
	# Calculate benchmarks and add them to file:
	for(b in 1:length(benchnames)){
		for(v in 1:length(varnames)){
			cat('Training benchmark',b,' for variable:',varnames[v],'\n')
			# Establish empirical model parameters on training data sets:
			trainedmodel = TrainEmpModel(ntrainsites,TrainPaths,
				benchx[[b]],varnames[v],benchparam[b],benchtype[b],
				removeflagged=removeflagged)
			# Use those parameters to make a prediction:
			cat('Predicting using benchmark',b,' \n')
			empprediction = PredictEmpFlux(PredictPathMet,benchx[[b]],
				varnames[v],trainedmodel)
			# Write this benchmark model to the netcdf file:
			cat('writing to netcdf file... \n')
			put.var.ncdf(ncid,paste(varnames[v],'_',benchnames[b],sep=''),
				vals=empprediction)
			# Save Qle, Qh and Qg data to construct Rnet:
			if(varnames[v]=='Qle'){
				BQle = empprediction
			}else if(varnames[v]=='Qh'){
				BQh = empprediction
			}else if(varnames[v]=='Qg'){
				BQg = empprediction
			}
		}
		# If all prerequisite variables exist, write Rnet:
		if(any(varnames=='Qle') & any(varnames=='Qh') & any(varnames=='Qg')){
			put.var.ncdf(ncid,paste('Rnet_',benchnames[b],sep=''),
				vals=(BQle+BQh+BQg))
		}
	}
	# Close benchmarks netcdf file:
	close.ncdf(ncid)
	
	return()
}

TrainEmpModel = function(nsites,infiles,xvarnames,yvarname,
	eparam,emod='kmeans',removeflagged=TRUE){
	library(ncdf) # load netcdf library
	if(length(infiles) != nsites){ # different # sites and files
		# Assume separate met and flux files:
		metfiles = 	infiles[1:nsites]
		fluxfiles = infiles[(nsites+1):length(infiles)]
	}else{
		metfiles = 	infiles
		fluxfiles = infiles
	}
	nxvars = length(xvarnames) # number of independent variables 
	# Determine if dependent variable is available at all requested sites:
	siteexclude = c()
	all_sites = TRUE # initialise
	ectr = 1
	for(s in 1:nsites){
		exists_var = FALSE # initialise
		fid = open.ncdf(fluxfiles[s])
		for (v in 1:fid$nvars){ # Search through all variables in netcdf file
			if(fid$var[[v]]$name==yvarname){
				exists_var=TRUE
				break
			}
		}
		close.ncdf(fid)
		if(! exists_var){
			siteexclude[ectr] = s
			all_sites = FALSE
			ectr = ectr + 1	
		}
	}
	if(! all_sites){ # if some sites didn't have the dependent variable (i.e. flux)
		cat('Sites',siteexclude,'do not have required variables and are being excluded. \n')
		# Exclude those sites from the training set:
		metfiles_new = c()
		fluxfiles_new = c()
		snewctr = 1
		for(s in 1:nsites){
			if(any(siteexclude==s)){
				# do not add site to new list 	
			}else{
				metfiles_new[snewctr] = metfiles[s]
				fluxfiles_new[snewctr] = fluxfiles[s]
				snewctr = snewctr + 1
			}
		}
		# Reduce number of sites in training set
		nsites = nsites - length(siteexclude)
		metfiles = metfiles_new
		fluxfiles = fluxfiles_new
	}else{
		cat('All sites have required variables. \n')	
	}
	
	# If none of the sites have the required variables, return.
	if( nsites <= 0 ) return(NULL)
	
	# Find out how many time steps in total training set:
	ntsteps = 0 # initialise number of time steps
	dstart = c() # index in all data vector of data set start
	dend = c() # index in all data vector of data set end
	for(s in 1:nsites){
		fid = open.ncdf(metfiles[s])
		timing = GetTimingNcfile(fid) # in PALS package
		close.ncdf(fid)
		dstart[s] = ntsteps + 1
		ntsteps = ntsteps + timing$tsteps
		dend[s] = ntsteps
	}
	yvar=c() # dependent variable
	xvar = matrix(NA,ntsteps,nxvars) # declare x variable data matrix
	if(removeflagged){
		xvar_qc = matrix(NA,ntsteps,nxvars)
		yvar_qc=c()
	}
	# Now poulate x-data matrix and y-data vector:
	for(s in 1:nsites){
		cat('Fetching data set ',s,'\n')
		# Get independent variables:
		for(v in 1:nxvars){
			# If we're using humidity as a benchmark input, change to relative humidity:
			if(xvarnames[v] != 'Qair'){
				tmpx = GetFluxnetVariable(xvarnames[v],metfiles[s],'blah')
				xvar[(dstart[s]:dend[s]),v] = tmpx$data
				
			}else{
				tmpx = GetFluxnetVariable(xvarnames[v],metfiles[s],'blah')
				tmpTair = GetFluxnetVariable('Tair',metfiles[s],'blah')
				tmpPSurf = GetFluxnetVariable('PSurf',metfiles[s],'blah')
				xvar[(dstart[s]:dend[s]),v] = Spec2RelHum(tmpx$data,tmpTair$data,tmpPSurf$data)
			}
			if(removeflagged & tmpx$qcexists){
				xvar_qc[(dstart[s]:dend[s]),v] = tmpx$qc
			}else if(removeflagged & ! tmpx$qcexists){
				# If no QC flag exists, assume data are original:
				xvar_qc[(dstart[s]:dend[s]),v] = 1
			}
		}
		tmpy = GetFluxnetVariable(yvarname,fluxfiles[s],'blah')
		yvar = c(yvar,tmpy$data)
		if(removeflagged & tmpy$qcexists){
			yvar_qc = c(yvar_qc,tmpy$qc)
		}else if(removeflagged & ! tmpy$qcexists){
			# If no QC flag exists, assume data are original:
			tmp = c(1:length(tmpy$data))*0 + 1
			yvar_qc = c(yvar_qc,tmp)
		}
	}
	# Construct training data set only from observed, and not 
	# gap-filled data, if requested:
	if(removeflagged){
		cat('Using only non-gap-filled data to train with... \n')
		# Convert to logical mask using default 1=>TRUE, 0=>FALSE 
		flagmask = (as.logical(xvar_qc[,1]) & as.logical(yvar_qc))
		if(nxvars>1){
			for(v in 1:(nxvars-1)){
				flagmask = (flagmask & as.logical(xvar_qc[,(v+1)]))
			}
		}
		xvar_new = xvar[flagmask,]
		yvar_new = yvar[flagmask]
		xvar = xvar_new
		yvar = yvar_new
		cat('...using',length(yvar_new),'of original',ntsteps,'time steps \n')
		ntsteps = length(yvar_new)
	}
	# Choose empirical model type:
	if(emod=='kmeans'){
		cat('Clustering',ntsteps,'time steps from',nsites,'sites... \n')
		# First sd-weight variables:
		wxvar = matrix(NA,ntsteps,nxvars) # declare x var data matrix
		xsd = c()
		xmn = c()
		for(v in 1:nxvars){
			xsd[v] = sd(xvar[,v])
			xmn[v] = mean(xvar[,v])
			wxvar[,v] = (xvar[,v]-xmn[v]) / xsd[v]
		}
		# Cluster dependent variables:
		xclst = kmeans(wxvar,eparam,iter.max = 50,nstart=3) # eparam assumed # clusters
		cat('At least',min(xclst$size),'data in each cluster \n')		
		cat('Regressing cluster number: ')
		intcpt = c()
		grad = matrix(NA,eparam,nxvars) # regression coefficients
		for(c in 1:eparam){
			cat(c,' ')
			tmp = lm(yvar[(xclst$cluster==c)]~
				xvar[(xclst$cluster==c),],na.action=na.omit)
			intcpt[c] = tmp$coefficients[1]
			grad[c,] = tmp$coefficients[2:(nxvars+1)]
		}
		cat('\n')				
	}else if(emod=='mlr'){
		# Perform linear regression
		intcpt = c()
		grad = matrix(NA,1,nxvars) # regression coefficients
		xclst = NA
		xsd = NA
		xmn=NA
		tmp = lm(yvar~xvar,na.action=na.omit)
		intcpt[1] = tmp$coefficients[1]
		grad[1,] = tmp$coefficients[2:(nxvars+1)]
	}else{
		CheckError('Unknown empirical model choice.')
	}
	empmod = list(xclst=xclst,grad=grad,int=intcpt,
		xsd=xsd,xmn=xmn,type=emod)
	return(empmod)
}

PredictEmpFlux = function(infile,xvarnames,yvarname,emod){
	# Predicts empirically based flux for a single site.
	library(ncdf) # load netcdf library
	nxvars = length(xvarnames) # number of independent variables
	# Check consistency of inputs to function:
	if(nxvars != length(emod$grad[1,])){
		CheckError('Number of dependent vars in call to EmpFlux inconsistent')
	}
	# Determine number of time steps in testing site data:
	fid = open.ncdf(infile)
	timing = GetTimingNcfile(fid) # in PALS package
	close.ncdf(fid)
	ntsteps = timing$tsteps
	yvar=c() # dependent variable
	xvar = matrix(NA,ntsteps,nxvars) # declare x var data matrix
	# Get test site dependent variable data:
	for(v in 1:nxvars){
		if(xvarnames[v] != 'Qair'){
			tmpx = GetFluxnetVariable(xvarnames[v],infile,'blah')
			xvar[,v] = tmpx$data
		}else{
			tmpx = GetFluxnetVariable(xvarnames[v],infile,'blah')
			tmpTair = GetFluxnetVariable('Tair',infile,'blah')
			tmpPSurf = GetFluxnetVariable('PSurf',infile,'blah')
			xvar[,v] = Spec2RelHum(tmpx$data,tmpTair$data,tmpPSurf$data)
		}		
	}
	if(emod$type=='kmeans'){
		cat('Sorting testing data into existing clusters... \n')
		nclst = length(emod$xclst$withinss)
		xdistall = array(NA,dim=c(ntsteps,nxvars,nclst))
		xdist = matrix(0,ntsteps,nclst)
		# Find distance of each time step to existing clusters:
		for(c in 1:nclst){
			for(v in 1:nxvars){
				xdistall[,v,c] = ((xvar[,v]-emod$xmn[v])/emod$xsd[v] - emod$xclst$centers[c,v])^2
				xdist[,c] = xdist[,c] + xdistall[,v,c]
			}
		}
		# For each time step, minvals is its distance to the cluster it belongs to:
		minvals = apply(xdist,1,function(x) min(x))
		cat('Constructing empirical flux estimate... \n')
		empflux = c()
		empflux[1:ntsteps] = 0
		# Construct empirically based flux timeseries using saved regression coefficients:
		for(c in 1:nclst){
			for(v in 1:nxvars){
				 empflux[(xdist[,c] == minvals)] = empflux[(xdist[,c] == minvals)] +
				 	xvar[(xdist[,c] == minvals),v]*emod$grad[c,v]
			}
			empflux[(xdist[,c] == minvals)] = empflux[(xdist[,c] == minvals)] + emod$int[c]	
		}
	}else if(emod$type=='mlr'){
		empflux = c()
		empflux = 0 # initialise
		for(v in 1:nxvars){
			empflux = empflux + emod$grad[1,v]*xvar[,v]
		}
		empflux = empflux + emod$int[1]		
	}
	return(empflux)
}