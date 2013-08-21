# LoadNetcdfVariable.R
#
# Reads a variable from netcdf file
# Gab Abramowitz UNSW 2012 (palshelp at gmail dot com)
#
# Reads a variable from an internally converted netcdf
# file of a flux tower site's observed data.
GetFluxnetVariable = function(varname,obsfile,units,flagonly=FALSE){
	library(ncdf) # load netcdf library	
	exists_var = FALSE
	exists_qc = FALSE
	qc=NA # initialise
	if(!file.exists(obsfile)){
		CheckError(paste('DS4: Data set file',obsfile,'does not exist.'))	
	}
	ofid=open.ncdf(obsfile,readunlim=FALSE) # open observed data file
	# Check variable exists in netcdf file:
	for (v in 1:ofid$nvars){ # Search through all variables in netcdf file
		if(ofid$var[[v]]$name==varname[1]){
			exists_var=TRUE
			if(exists_var & exists_qc) {break}
		}
		if(ofid$var[[v]]$name==paste(varname[1],'_qc',sep='')){
			exists_qc=TRUE
			if(exists_var & exists_qc) {break}	
		}
	}
	# If not, return:
	if(! exists_var){
		dsetname = att.get.ncdf(ofid,varid=0,attname='PALS_dataset_name')
		if(dsetname$hasatt){
			CheckError(paste('DS2: Variable \"',varname[1],
				'\" does not exist in data set ',dsetname$value,
				'; file:',stripFilename(obsfile),sep=''))
		}else{
			CheckError(paste('DS2: Variable \"',varname[1],
				'\" does not exist in file:',stripFilename(obsfile),sep=''))
		}
	}
	# Read QC data if it exists:
	if(exists_qc){
		qc=get.var.ncdf(ofid,paste(varname[1],'_qc',sep=''))
	}
	if(! flagonly){ # if this function call is actually about fetching data:
		timing = GetTimingNcfile(ofid)
		data=get.var.ncdf(ofid,varname[1])   # read observed variable data
		obs=list(data=data,timing=timing,qc=qc,qcexists=exists_qc)
	}else{
		obs=list(qcexists=exists_qc,qc=qc)
	}
	close.ncdf(ofid) # close netcdf file	
	return(obs)
}
# This function is the model output read interface.
# Inputs: variable name; model file name; variable units.
GetModelOutput = function(varname,modelfile,units){
	library(ncdf) # load netcdf library
	if(!file.exists(modelfile)){
		CheckError(paste('M4: Model output file',modelfile,'does not exist.'))	
	}
	mfid=open.ncdf(modelfile,readunlim=FALSE) # open model file
	# Find appropriate variable name for this model output,
	# and any units conversion required:
	modelvar = GetModelVariableName(mfid,varname,units)
	# Get time step size:
	modeltiming = GetTimingNcfile(mfid)
	# Get model data:	
	# Check for special cases first:
	if(modelvar$name=='FCEV'){ # lat heat in CLM has 3 components
		data1=get.var.ncdf(mfid,'FCEV') # read canopy evap
		data2=get.var.ncdf(mfid,'FCTR') # read canopy transp
		data3=get.var.ncdf(mfid,'FGEV') # read ground evap
		data = data1 + data2 + data3
	}else{ # otherwise just fetch variable data:
		data=get.var.ncdf(mfid,modelvar$name) # read model output data
	}
	# Apply any units changes:
	data = data*modelvar$multiplier + modelvar$addition
	close.ncdf(mfid) # close netcdf file
	model=list(data=data,timing = modeltiming)
	return(model)
}

GetBenchmarkVariable = function(varname,benchfile){
	library(ncdf) # load netcdf library
	if(!file.exists(benchfile)){
		CheckError(paste('B5: Benchmark file',benchfile,'does not exist.'))	
	}
	bfid=open.ncdf(benchfile,readunlim=FALSE) # open benchmark file
	benchtiming = GetTimingNcfile(bfid) # get timing details
	# Return null result if variable doesn't exist:
	if(!NcvarExists(bfid,varname[1])){
		return(NULL)	
	}
	data=get.var.ncdf(bfid,varname) # read model output data
	close.ncdf(bfid) # close netcdf file
	bench=list(data=data,timing=benchtiming)
	return(bench)
}

NcvarExists = function(fid,varname){
	# Checks that variable exists in netcdf file, and additionally 
	# checks whether quality control couterpart variable exists:
	exists_var = FALSE
	for (v in 1:fid$nvars){ # Search through all variables in netcdf file
		if(fid$var[[v]]$name==varname){
			exists_var=TRUE
			break
		}
	}
	return(exists_var)	
}