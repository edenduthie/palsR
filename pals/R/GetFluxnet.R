# GetFluxnet.R
#
# Reads a fluxnet variable from netcdf file
# Gab Abramowitz UNSW 2014 (palshelp at gmail dot com)
#
# Reads a variable from an internally converted netcdf
# file of a flux tower site's observed data.
GetFluxnetVariable = function(variable,filedetails,flagonly=FALSE){
	# 'variable' - list from GetVariableDetails
	# 'filedetails' - list containing path, mimetype, component etc
	library(ncdf4) # load netcdf library	
	exists_var = FALSE
	exists_qc = FALSE
	qc=NA # initialise
	errtext='ok'
	# Return if file does not exist:
	if(!file.exists(filedetails[['path']])){
		errtext = paste('DS4: Data set file',filedetails[['name']],
			'at',filedetails[['path']],'does not exist.')
		obs=list(errtext=errtext,err=TRUE,exists=FALSE,qcexists=FALSE)
		return(obs)
	}
	# Open observed data file:
	fid=nc_open(filedetails[['path']],write=FALSE,readunlim=FALSE)
	# Check required variable exists:
	vexists = NcvarExists(fid,variable[['Name']][1])
	# If not, return with error:
	if(! vexists$var){
		dsetname = ncatt_get(fid,varid=0,attname='PALS_dataset_name')
		if(dsetname$hasatt){
			errtext = paste('DS2: Variable ',variable[['Name']][1],
				' does not exist in data set ',filedetails[['name']],sep='')
			obs=list(errtext=errtext,exists=FALSE,qcexists=FALSE,err=TRUE)
			fid = nc_close(fid)
			return(obs)
		}else{
			errtest = paste('DS2: Variable ',variable[['Name']][1],
				'does not exist in data set',filedetails[['name']],sep='')
			obs=list(errtext=errtext,exists=FALSE,err=TRUE,qcexists=FALSE)
			fid = nc_close(fid)
			return(obs)
		}
	}
	# Read QC data if it exists:
	if(vexists$qc){
		qc=ncvar_get(fid,paste(variable[['Name']][1],'_qc',sep=''))
	}
	if(! flagonly){ # if this function call is actually about fetching data:
		timing = GetTimingNcfile(fid)
		data=ncvar_get(fid,variable[['Name']][1])   # read observed variable data
		latlon = GetLatLon(fid)
		obs=list(data=data,timing=timing,qc=qc,qcexists=vexists$qc,name=filedetails$name,
			grid=latlon,err=FALSE,errtext=errtext)
	}else{
		obs=list(qcexists=vexists$qc,qc=qc,name=filedetails$name,
			err=FALSE,errtext=errtext)
	}
	nc_close(fid) # close netcdf file	
	return(obs)
}
