# LoadNetcdfVariable.R
#
# Reads a variable from netcdf file
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
			return(obs)
		}else{
			errtest = paste('DS2: Variable ',variable[['Name']][1],
				'does not exist in data set',filedetails[['name']],sep='')
			obs=list(errtext=errtext,exists=FALSE,err=TRUE,qcexists=FALSE)
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
		obs=list(data=data,timing=timing,qc=qc,qcexists=vexists$qc,name=filedetails$name,
			err=FALSE,errtext=errtext)
	}else{
		obs=list(qcexists=vexists$qc,qc=qc,name=filedetails$name,
			err=FALSE,errtext=errtext)
	}
	nc_close(fid) # close netcdf file	
	return(obs)
}

# This function reads netcdf model output.
GetModelOutput = function(variable,filelist){
	library(ncdf4) # load netcdf library
	errtext='ok'	
	# only single file MOs at the moment:
	if(length(filelist) != 1){
		errtext = paste('M5: Cannot yet deal with Model Outputs in multiple files. MO:',
			filelist[[1]][['name']])
		model=list(errtext=errtext,err=TRUE)
		return(model)
	}
	# Return error if file does not exist:
	if(!file.exists(filelist[[1]][['path']])){
		errtext = paste('M4: Model output file',filelist[[1]][['path']],'does not exist.')
		model=list(errtext=errtext,err=TRUE)
		return(model)	
	}
	# Open model output file
	mfid=nc_open(filelist[[1]][['path']],write=FALSE,readunlim=FALSE)
	# Check alternative names to make sure variable exits:
	exists = AnyNcvarExists(mfid,variable[['Name']])
	# If not, return error:
	if( ! exists$var){
		errtext = paste('M3: Requested variable',variable[['Name']][1],
			'does not appear to exist in Model Ouput:',
			filelist[[1]][['name']])
		model=list(errtext=errtext,err=TRUE)
		return(model)
	}
	units = CheckNcvarUnits(mfid,variable[['Name']][exists$index],variable,filelist[[1]][['path']])
	# If units issue, return error:
	if(units$err){
		model=list(errtext=units$errtext,err=TRUE)
		return(model)
	}
	# Get time step size:
	modeltiming = GetTimingNcfile(mfid)
	# Get model data:	
	# Check for special cases first:
	if(mfid$var[[exists$index]]$name=='FCEV'){ # lat heat in CLM has 3 components
		data1=ncvar_get(mfid,'FCEV') # read canopy evap
		data2=ncvar_get(mfid,'FCTR') # read canopy transp
		data3=ncvar_get(mfid,'FGEV') # read ground evap
		vdata = data1 + data2 + data3
		rm(data1,data2,data3)
	}else{ # otherwise just fetch variable data:
		vdata=ncvar_get(mfid,(variable[['Name']][exists$index])) # read model output data	
	}
	# Apply any units changes:
	vdata = vdata*units$multiplier + units$addition
	nc_close(mfid) # close netcdf file
	model=list(data=vdata,timing = modeltiming,name=filelist[[1]]$name,
		err=FALSE,errtext=errtext)
	return(model)
}

GetBenchmarks = function(variable,filelist,nBench){
	# Collects the model outputs that form benchmark simulations as a list
	errtext = 'ok'
	bench = list()
	if(nBench$number > 0){
		# Save for nBench$number positions in list for benchmark data;
		# they'll be filled shortly:
		for(b in 1:nBench$number){
			bench[[b]] = 0
		}
    	bench[['exist']] = TRUE
    	bench[['howmany']] = 0
    	bench[['index']] = c()
    	bench[['errtext']] = ' '
    	for(b in 1:nBench$number){
    		bench[['howmany']] = bench[['howmany']] + 1
    		# Select those files in file list that correspond to benchmark 'b'
    		thisbenchfiles = list()
    		for(f in 1:length(nBench$benchfiles[[b]])){
    			thisbenchfiles[[f]] = filelist[[ nBench$benchfiles[[b]][f] ]]
    		}
	    	bench[[b]] = GetModelOutput(variable,thisbenchfiles)
	    	if(bench[[b]]$err){ # i.e. there was an error of some sort retrieving benchmark
	    		# Add to this benchamrk's errtext field - note it's a benchmark:
	    		bench[[b]]$errtext = paste('B1: Benchmark error:',bench[[b]]$errtext)
	    		bench[['errtext']] = paste(bench[['errtext']],'B1: Benchmark',b,':',bench[[b]]$errtext)
	    		# decrease the number of benchmarks available for this variable:
	    		bench[['howmany']] = bench[['howmany']] - 1
	    	}else{
	    		# Note which benchmarks didn't fail:
	    		bench[['index']] = c(bench[['index']],b)
	    	}	
	    }
	    # If reading of all benchmarks failed, note that there are none:
	    if(bench[['howmany']] == 0){
	    	bench[['exist']] = FALSE	
	    }
	}else{
		bench[['exist']] = FALSE
		bench[['howmany']] = 0
		bench[['errtext']] = 'B5: No user nominated benchmarks '
	}
	return(bench)
}

NcvarExists = function(fid,varname){
	# Checks that variable exists in netcdf file, and additionally 
	# checks whether quality control couterpart variable exists:
	exists_var = FALSE
	exists_qc = FALSE
	for (v in 1:fid$nvars){ # Search through all variables in netcdf file
		if(fid$var[[v]]$name==varname){
			exists_var=TRUE
			if(exists_var & exists_qc) {break}
		}else if(fid$var[[v]]$name==paste(varname,'_qc',sep='')){
			exists_qc=TRUE
			if(exists_var & exists_qc) {break}	
		}
	}
	ncvexists = list(var=exists_var,qc=exists_qc)
	return(ncvexists)	
}

AnyNcvarExists = function(fid,varnames){
	# Checks whether a variable exists as any of its alternative standard names.
	exists_var = FALSE
	exists_qc = FALSE
	vindex = 0
	for(v in 1:length(varnames)){
		exists = NcvarExists(fid,varnames[v])
		if(exists$var){
			exists_var=TRUE
			exists_qc = exists$qc
			vindex = v
			break
		}	
	}
	ncvexists = list(var=exists_var,qc=exists_qc,index=vindex)
	return(ncvexists)	
}

CheckNcvarUnits = function(fid,vname,variable,file){
	# Check whether units exist for a variable, and if so, whether
	# they correspond to any standard version of units text:
	# Get units for variable
	errtext='ok'
	mvunits=ncatt_get(fid,vname,'units')
	if(! mvunits$hasatt){
		errtext = paste('Variable',vname,'in file',
			file,'does not have a units attribute.')
		units = list(errtext = errtext, err=TRUE)
		return(units)
	}
	UnitsExist = TRUE
	UnitsMatch = FALSE
	for (u in 1:length(variable$UnitsName)){
		if(mvunits$value==variable$UnitsName[u]){ # i.e. found units match
			UnitsMatch = TRUE
			# Set units adjustments appropriately:
			multiplier = variable$Multiplier[u]
			addition = variable$Addition[u]
			break # out of units name for loop
		}
	}
	if( ! UnitsMatch){ # i.e. didn't recognise variable units
		units=mvunits$value
		errtext = paste('M3: Did not recognise units',mvunits$value,'for',
			fid$var[[v]]$name,'in model output file',file)
		units = list(errtext = errtext, err=TRUE)
		return(units)	
	}
	units = list(exist = UnitsExist, match=UnitsMatch, value = mvunits$value,
		multiplier=multiplier, addition=addition,err=FALSE,errtext=errtext)
	return(units)
}