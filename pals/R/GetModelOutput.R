# GetModelOutput.R
#
# Reads a variable from model output netcdf file
# Gab Abramowitz UNSW 2014 (palshelp at gmail dot com)
#
GetModelOutput = function(variable,filelist){
	library(ncdf4) # load netcdf library
	errtext='ok'
	
	mfid=list() # initialise netcdf file ID list
	modeltiming = list() # initialise list of each file's timing details
	
	# First check that each file exists, contains the variable we want, has known units,
	# and known timing structure:
	for(f in 1:length(filelist)){ # For each file of this MO:
		# Check file exists:
		if(!file.exists(filelist[[f]][['path']])){
			errtext = paste('M4: Model output file',filelist[[f]][['path']],'does not exist.')
			model=list(errtext=errtext,err=TRUE)
			return(model)	
		}
		# Open model output file
		mfid[[f]]=nc_open(filelist[[f]][['path']],write=FALSE,readunlim=FALSE)
		# Check that requested variable exists:
		exists = AnyNcvarExists(mfid[[f]],variable[['Name']])
		if( ! exists$var){
			errtext = paste('M3: Requested variable',variable[['Name']][1],
				'does not appear to exist in Model Ouput:', filelist[[f]][['name']])
			model=list(errtext=errtext,err=TRUE)
			mfid = lapply(mfid, nc_close)
			return(model)
		}
		# Check variable units are known:
		units = CheckNcvarUnits(mfid[[f]],variable[['Name']][exists$index],variable,filelist[[f]][['path']])
		# If units issue, return error:
		if(units$err){
			model=list(errtext=units$errtext,err=TRUE)
			mfid = lapply(mfid, nc_close)
			return(model)
		}
		# Get timing details for each file:
		modeltiming[[f]] = GetTimingNcfile(mfid[[f]])
		# If timing issue, return error:
		if(modeltiming[[f]]$err){
			model=list(errtext=modeltiming[[f]]$errtext,err=TRUE)
			mfid = lapply(mfid, nc_close)
			return(model)
		}
	}
	
	# Now try to ascertain how the data in these files needs to be assembled:
	allintervals = c()
	alltsteps = c()
	allyear = c()
	for(f in 1:length(filelist)){
		allintervals[f] = modeltiming[[f]]$interval
		alltsteps[f] = modeltiming[[f]]$tsteps
		allyear[f] = modeltiming[[f]]$syear
	}
	if((all(allintervals == 'monthly')) && (all(alltsteps == 12))){		
		# i.e. all are monthly data in year length files
		nyears = length(filelist)
		ntsteps = 12 * nyears # total number of time steps
		# Get lat / lon from file
		latlon = GetLatLon(mfid[[1]])
		if(latlon$err){	
			model = list(err=TRUE,errtext=latlon$errtext)
			mfid = lapply(mfid, nc_close)
			return(model)
		}
		# Check that MO files don't repeat years:
		if(length(unique(allyear)) != length(allyear)){ # i.e. a repeated year has been removed
			errtext='Model output has two files with the same starting year.'
			model = list(err=TRUE,errtext=errtext)
			mfid = lapply(mfid, nc_close)
			return(model)
		}
		
		# Allocate space for data:
		vdata = array(NA,dim=c(latlon$lonlen,latlon$latlen,ntsteps) )
		# Define the order to read files:
		fileorder = order(allyear)
		
		# Check there are no gaps in years:
		if(nyears>1){
			descending_years = rev(allyear[fileorder])
			gap_bw_files = descending_years[1:(nyears-1)] - descending_years[2:nyears]
			if(any(gap_bw_files != 1)){
				errtext='Model output is missing some years.'
				model = list(err=TRUE,errtext=errtext)
				mfid = lapply(mfid, nc_close)
				return(model)
			}
		}
		
		# Get data:
		for(f in 1:length(filelist)){ # For each file sent by js
			vdata[,,((f-1)*12+1) : ((f-1)*12+12)] = ncvar_get(mfid[[ fileorder[f] ]],variable[['Name']][exists$index])
		}
		# Create model timing list to reflect aggregated data:
		modeltimingall = list(tstepsize=modeltiming[[1]]$tstepsize,tsteps=ntsteps,
			syear=modeltiming[[1]]$syear,smonth=modeltiming[[1]]$smonth,sdoy=modeltiming[[1]]$sdoy,
			interval=modeltiming[[1]]$interval)
	}else if((all(allintervals == 'timestep')) && (all(alltsteps == alltsteps[1]))){
		# i.e. all are per time step data with the same number of time steps in each file
		ntsteps = alltsteps[1] * length(filelist) # total number of time steps
		tsteps1 = alltsteps[1]
		# Get lat / lon from file
		latlon = GetLatLon(mfid[[1]])
		if(latlon$err){	
			model = list(err=TRUE,errtext=latlon$errtext)
			mfid = lapply(mfid, nc_close)
			return(model)
		}
		# Allocate space for data:
		vdata = array(NA,dim=c(latlon$lonlen,latlon$latlen,ntsteps) )
		# Define the order to read files:
		fileorder = order(allyear)
		# Get data:
		if(mfid[[1]]$var[[exists$index]]$name=='FCEV'){ # lat heat in CLM has 3 components
			for(f in 1:length(filelist)){ # For each file sent by js
				vdata[,,((f-1)*tsteps1+1) : ((f-1)*tsteps1+tsteps1)] = 
					ncvar_get(mfid[[ fileorder[f] ]],'FCEV')
				vdata[,,((f-1)*tsteps1+1) : ((f-1)*tsteps1+tsteps1)] = 
					vdata[,,((f-1)*tsteps1+1) : ((f-1)*tsteps1+tsteps1)] +
					ncvar_get(mfid[[ fileorder[f] ]],'FCTR')
				vdata[,,((f-1)*tsteps1+1) : ((f-1)*tsteps1+tsteps1)] = 
					vdata[,,((f-1)*tsteps1+1) : ((f-1)*tsteps1+tsteps1)] + 
					ncvar_get(mfid[[ fileorder[f] ]],'FGEV')
			}
		}else{
			for(f in 1:length(filelist)){ # For each file sent by js
				vdata_tmp = ncvar_get(mfid[[ fileorder[f] ]],variable[['Name']][exists$index],collapse_degen=FALSE)

				if ((variable[['Name']][1]=='NEE') & (length(vdata_tmp) != (ntsteps*latlon$lonlen*latlon$latlen))) {
					# likely an ORCHIDEE file where NEE has dim (x,y,t,vegtype), in which case sum over
					# vegtype dim - NEE values are already weighted by vegtype fraction:
					vdata_tmp = apply(vdata_tmp,c(1,2,4),sum)
				}
				if(length(vdata_tmp) != (ntsteps*latlon$lonlen*latlon$latlen)){
					errtext = paste('Requested variable',variable[['Name']][1],
							'has more dimensions than expected in Model Ouput:', filelist[[f]][['name']])
					model = list(err=TRUE,errtext=errtext)
					mfid = lapply(mfid, nc_close)
					return(model)
				}

				vdata[,,((f-1)*tsteps1+1) : ((f-1)*tsteps1+tsteps1)] = vdata_tmp
			}
		}
		# Create model timing list to reflect aggregated data:
		modeltimingall = list(tstepsize=modeltiming[[1]]$tstepsize,tsteps=ntsteps,
			syear=modeltiming[[1]]$syear,smonth=modeltiming[[1]]$smonth,sdoy=modeltiming[[1]]$sdoy,
			interval=modeltiming[[1]]$interval)
	}else{
		errtext='PALS is not currently able to read model output files with this timing structure.'
		model = list(err=TRUE,errtext=errtext)
		mfid = lapply(mfid, nc_close)
		return(model)
	}
	
	# Close all files for this model output:
	mfid = lapply(mfid, nc_close)
	
	# Apply any units changes:
	vdata = vdata*units$multiplier + units$addition
	# Create list to return from function:	
	model=list(data=vdata,timing = modeltimingall,name=filelist[[1]]$name,
		err=FALSE,errtext=errtext)
	return(model)
}

GetLatLon = function(mfid){
	# Gets the latitude and longitide dimensions from a model output netcdf file
	errtext='ok'
	# Fetch accepted names for latitude:
	lat_det = GetVariableDetails('lat')
	# Check an acceptable latitude variable exists:
	exists_lat = AnyNcvarExists(mfid,lat_det[[1]][['Name']])
	if(! exists_lat$var){
		errtext = paste('Could not find latitude variable in',stripFilename(mfid$filename))
		latlon = list(err=TRUE,errtext=errtext)
		return(latlon)
	}
	# Now fetch latitude information:
	if(exists_lat$dimvar){
		# either as a dimension variable, if that was the found acceptable name:
		lat = mfid$dim[[exists_lat$dimnum]]$vals
		latlen = mfid$dim[[exists_lat$dimnum]]$len
	}else{
		# or as a normal variable:
		if(mfid$var[[exists_lat$varnum]]$ndims != 1){ # If not a 1D latitude variable
			# Check that it's still a lat-lon grid:
			lat = ncvar_get(mfid,lat_det[[1]][['Name']][exists_lat$index]) # get lat data
			latlen = length(unique(as.vector(lat))) # i.e. the number of unique latitude values
			if(any(mfid$var[exists_lat$index]$size != latlen)){
				# i.e. number of unique lat vals is not equal to the length of either of the dims
				# on which latitude depends
				errtext = 'PALS cannot yet cope with non lat-lon grids yet.'	
				latlon = list(err=TRUE,errtext=errtext)
				return(latlon)
			}
		}
	}
	
	# Repeat the process for longitude:
	# Fetch accepted names for longitude:
	lon_det = GetVariableDetails('lon')
	# Check an acceptable longitude variable exists:
	exists_lon = AnyNcvarExists(mfid,lon_det[[1]][['Name']])
	if(! exists_lon$var){
		errtext = paste('Could not find longitude variable in',stripFilename(mfid$filename))
		latlon = list(err=TRUE,errtext=errtext)
		return(latlon)
	}
	# Now fetch longitude information:
	if(exists_lon$dimvar){
		# either as a dimension variable, if that was the found acceptable name:
		lon = mfid$dim[[exists_lon$dimnum]]$vals
		lonlen = mfid$dim[[exists_lon$dimnum]]$len
	}else{
		# or as a normal variable:
		if(mfid$var[[exists_lon$varnum]]$ndims != 1){ # If not a 1D latitude variable
			# Check that it's still a lat-lon grid:
			lon = ncvar_get(mfid,lon_det[[1]][['Name']][exists_lon$index]) # get lon data
			lonlen = length(unique(as.vector(lon))) # i.e. the number of unique latitude values
			if(any(mfid$var[exists_lat$index]$size != latlen)){
				# i.e. number of unique lat vals is not equal to the length of either of the dims
				# on which latitude depends
				errtext = 'PALS cannot yet cope with non lat-lon grids yet.'	
				latlon = list(err=TRUE,errtext=errtext)
				return(latlon)
			}
		}
	}
	# Return result:
	latlon = list(err=FALSE,errtext=errtext,lat=lat,lon=lon,latlen=latlen,lonlen=lonlen)
	return(latlon)
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
	    		bench[[b]]$errtext = paste('Benchmark error: ',bench[[b]]$errtext,sep='')
	    		bench[['errtext']] = paste(bench[['errtext']],'Benchmark ',b,': ',bench[[b]]$errtext,sep='')
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
		bench[['errtext']] = 'No user nominated benchmarks.'
	}
	return(bench)
}

NcvarExists = function(fid,varname){
	# Checks that variable exists in netcdf file, and additionally 
	# checks whether quality control couterpart variable exists:
	exists_var = FALSE
	exists_qc = FALSE
	varnum = NULL
	dimnum = NULL
	dimvar = FALSE
	for (v in 1:fid$nvars){ # Search through all variables in netcdf file
		if(fid$var[[v]]$name==varname){
			exists_var=TRUE
			varnum = v
			if(exists_var & exists_qc) {break}
		}else if(fid$var[[v]]$name==paste(varname,'_qc',sep='')){
			exists_qc=TRUE
			if(exists_var & exists_qc) {break}	
		}
	}
	# If not found as a variable, it may be a dimension variable:
	if(! exists_var){
		for(d in 1:length(fid$dim)){
			# If a dimension name & dimension variable
			if(fid$dim[[d]]$name == varname){
				if(fid$dim[[d]]$dimvarid$id != -1){# i.e. dim var exists
					exists_var=TRUE
					dimvar = TRUE
					dimnum = d
				}
			}
		}
	}
	ncvexists = list(var=exists_var,qc=exists_qc,dimvar=dimvar,dimnum=dimnum,varnum=varnum)
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
	ncvexists = list(var=exists_var,qc=exists_qc,index=vindex,dimvar=exists$dimvar,
		dimnum=exists$dimnum,varnum=exists$varnum)
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