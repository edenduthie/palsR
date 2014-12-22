# NetcdfCompatibility.R
#
# Functions that check netcdf compatibility, obs-model compatibility
# Gab Abramowitz UNSW 2014 (palshelp at gmail dot com)
#
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

CanAnalysisProceed = function(obs,model){
	# Checks obs, model variables were found, timing is appropriate, and spatial grids match
		
	# Check for obs or model aren't missing variable data:
	readcheck = CheckDataRead(obs$err,obs$errtext,model$err,model$errtext)
	# Don't proceed and report error if there's an issue:		
	if(readcheck$err){
		return(readcheck)
	}	
	# Check model, obs timing consistency
	tcheck = CheckTiming(obs$timing,model$timing)	
	# Don't proceed and report error if there's an issue:	
	if(tcheck$err){
		return(tcheck)
	}
	proceed = list(err=FALSE)
	return(proceed)
}

CheckDataRead = function(obserr,obserrtext,moderr,moderrtext){
	# Simply reports whether there was a read error from either
	# obs or model output reading (no file, appropriate variable etc)
	errtext = 'ok'
	err = FALSE
	if(obserr){
		err = TRUE
		errtext = obserrtext
	}else if(moderr){
		err = TRUE
		errtext = moderrtext
	}	
	result = list(err = err,errtext = errtext)
	return(result)
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
		errtext = paste('Did not recognise units',mvunits$value,'for',
			fid$var[[v]]$name,'in model output file',file)
		units = list(errtext = errtext, err=TRUE)
		return(units)	
	}
	units = list(exist = UnitsExist, match=UnitsMatch, value = mvunits$value,
		multiplier=multiplier, addition=addition,err=FALSE,errtext=errtext)
	return(units)
}