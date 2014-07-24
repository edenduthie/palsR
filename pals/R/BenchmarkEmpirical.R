# BenchmarkEmpirical.R
#
# This set of functions is used for training empirical models used to benchmark
# LSM simulations as well as using these trained models to perform benchmark 
# simulations, and save the results in an ALMA format netcdf output file.
#
# Gab Abramowitz CCRC, UNSW 2014 (palshelp at gmail dot com)
#

GenerateEmpBenchmark = function(TrainPathsMet,TrainPathsFlux,PredictPathMet,PredictNcPath,
	varnames,varsunits,removeflagged,binfo,outofsample){
	
	library(ncdf4) # load netcdf library
	
	ntrainsites = length(TrainPathsMet) # number of training sites
	
	# Define netcdf file for storing benchmark simulations:
	missing_value=NcMissingVal # default missing value for all variables
	# Define x, y dimensions
	xd = ncdim_def('x',vals=c(1),units='')	
	yd = ncdim_def('y',vals=c(1),units='')
	# Open met prediction file to copy timing and global attribute details:
	mfid=nc_open(PredictPathMet,readunlim=FALSE,write=FALSE)
	timeunits=ncatt_get(mfid,'time','units')
	timedata=ncvar_get(mfid,'time')
	DataSetName=ncatt_get(mfid,varid=0,'PALS_dataset_name')
	DataSetVersion=ncatt_get(mfid,varid=0,'PALS_dataset_version')
	Lat=ncvar_get(mfid,'latitude')
	Lon=ncvar_get(mfid,'longitude')
	nc_close(mfid)
	
	# Define time dimension:
	td = ncdim_def('time',unlim=TRUE,units=timeunits$value,vals=timedata)
	# Begin defining variables (time independent first):
	benchvars = list() # initialise
	# Define latitude variable:
	benchvars[[1]] = ncvar_def('latitude',units='degrees_north',
		dim=list(xd,yd),missval=missing_value,longname='Latitude')
	# Define longitude variable:
	benchvars[[2]] = ncvar_def('longitude',units='degrees_east',
		dim=list(xd,yd),missval=missing_value,longname='Longitude')
	# Define time varying benchmark variables:
	for(v in 1:length(varnames)){
		benchvars[[(v+2)]]=ncvar_def(name=varnames[v],units=varsunits[v],
			dim=list(xd,yd,td),missval=missing_value,
			longname=paste('Empirical',varnames[v],'benchmark timeseries'))
	}
		
	# Create benchmark netcdf file:
	ncid = nc_create(filename=PredictNcPath,vars=benchvars)
	
	# Add ancillary details about benchmark:
	# First just create string of input variable names:
	benchin = ''
	cnt = 1
	repeat{
		benchin = paste(benchin,binfo$x[cnt],sep='')
		if(length(binfo$x) == cnt){ 
			break 
		}else{
			benchin = paste(benchin,', ',sep='')
		}
		cnt = cnt + 1
	}
	# Then save this string as benchmark_inputs attribute and
	# add empirical model type attribute:
	for(v in 1:length(varnames)){
		ncatt_put(ncid,varnames[v],'benchmark_inputs',benchin)
		ncatt_put(ncid,varnames[v],'benchmark_modeltype',binfo$type)
	}
	
	# Write global attributes:
	ncatt_put(ncid,varid=0,attname='Production_time',attval=as.character(Sys.time()))
	ncatt_put(ncid,varid=0,attname='Production_source',
		attval='Empirical benchmark simulation')
	if(removeflagged){ # Note if gap-filled data has been excluded 
		ncatt_put(ncid,varid=0,attname='Exclusions',
 			attval='Empirical models trained only on non-gap-filled data')
	}
	if(outofsample){
		ncatt_put(ncid,varid=0,attname='Out-of-sample',
 			attval=paste(DataSetName,'data was NOT used in training regression parameters for this prediction.'))
	}
	ncatt_put(ncid,varid=0,attname='PALS_dataset_name',attval=DataSetName)
	ncatt_put(ncid,varid=0,attname='PALS_dataset_version',attval=DataSetVersion)
	ncatt_put(ncid,varid=0,attname='Contact',attval='palshelp@gmail.com')
	# Add time-independent variable data to file:
	ncvar_put(ncid,'latitude',vals=Lat)
	ncvar_put(ncid,'longitude',vals=Lon)
	# Create paths to training data files:
	TrainPaths = c(TrainPathsMet,TrainPathsFlux)
	# Calculate benchmarks and add them to file:
	for(v in 1:length(varnames)){
		cat('Training benchmark',binfo$type,' for variable:',varnames[v],'\n')
		# Establish empirical model parameters on training data sets:
		trainedmodel = TrainEmpModel(ntrainsites,TrainPaths,binfo$x,varnames[v],
			binfo$par[b],binfo$type[b],removeflagged=removeflagged)
		# Use those parameters to make a prediction:
		cat('...Predicting using benchmark \n')
		empprediction = PredictEmpFlux(PredictPathMet,binfo$x,
			varnames[v],trainedmodel)
		# Write this benchmark model to the netcdf file:
		cat('...writing to netcdf file \n')
		ncvar_put(ncid,varnames[v],vals=empprediction)
	}
	# Close benchmarks netcdf file:
	nc_close(ncid)
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