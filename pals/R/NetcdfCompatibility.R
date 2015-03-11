# NetcdfCompatibility.R
#
# Functions that check netcdf compatibility, obs-model compatibility
# Gab Abramowitz UNSW 2014 (palshelp at gmail dot com)
#
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
	# Check model, obs grid consistency
	gcheck = CheckGrids(obs$grid,model$grid)	
	# Don't proceed and report error if there's an issue:	
	if(gcheck$err){
		return(gcheck)
	}
	proceed = list(err=FALSE)
	return(proceed)
}

GetGrid = function(fid){
	# Gets spatial grid information from a netcdf file
	errtext='ok'
	# Fetch accepted names for latitude:
	lat_det = GetVariableDetails('lat')
	# Check an acceptable latitude variable exists:
	exists_lat = AnyNcvarExists(fid,lat_det[[1]][['Name']])
	if(! exists_lat$var){
		errtext = paste('Could not find latitude variable in',stripFilename(fid$filename))
		latlon = list(err=TRUE,errtext=errtext)
		return(latlon)
	}
	# Now fetch latitude information:
	if(exists_lat$dimvar){
		# either as a dimension variable, if that was the found acceptable name:
		lat = fid$dim[[exists_lat$dimnum]]$vals
		latlen = fid$dim[[exists_lat$dimnum]]$len
	}else{
		# or as a normal variable:
		if(fid$var[[exists_lat$varnum]]$ndims != 1){ # If not a 1D latitude variable
			# Check that it's still a lat-lon grid:
			lat = ncvar_get(fid,lat_det[[1]][['Name']][exists_lat$index]) # get lat data
			latlen = length(unique(as.vector(lat))) # i.e. the number of unique latitude values
			if(any(fid$var[exists_lat$index]$size != latlen)){
				# i.e. number of unique lat vals is not equal to the length of either of the dims
				# on which latitude depends
				errtext = 'PALS cannot yet cope with non rectilinear grids yet.'	
				latlon = list(err=TRUE,errtext=errtext)
				return(latlon)
			}
		}
	}
	
	# Repeat the process for longitude:
	# Fetch accepted names for longitude:
	lon_det = GetVariableDetails('lon')
	# Check an acceptable longitude variable exists:
	exists_lon = AnyNcvarExists(fid,lon_det[[1]][['Name']])
	if(! exists_lon$var){
		errtext = paste('Could not find longitude variable in',stripFilename(fid$filename))
		latlon = list(err=TRUE,errtext=errtext)
		return(latlon)
	}
	# Now fetch longitude information:
	if(exists_lon$dimvar){
		# either as a dimension variable, if that was the found acceptable name:
		lon = fid$dim[[exists_lon$dimnum]]$vals
		lonlen = fid$dim[[exists_lon$dimnum]]$len
	}else{
		# or as a normal variable:
		if(fid$var[[exists_lon$varnum]]$ndims != 1){ # If not a 1D latitude variable
			# Check that it's still a lat-lon grid:
			lon = ncvar_get(fid,lon_det[[1]][['Name']][exists_lon$index]) # get lon data
			lonlen = length(unique(as.vector(lon))) # i.e. the number of unique latitude values
			if(any(fid$var[exists_lat$index]$size != latlen)){
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

CheckGrids = function(obs,mod){
	# Checks compatibility of observed and model output spatial grids
	errtext = 'ok'
	err = FALSE
#	if(any(obs$lat != mod$lat) | any(obs$lon != mod$lon)){
#		err=TRUE
#		errtext = 'Spatial grids incompatible between obs and modelled data.'	
#	}
	result = list(err = err,errtext = errtext)
	return(result)
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

GetVariableDetails = function(request_names){
	variable_list = list()
	for(v in 1:length(request_names)){
		var_details = list()
		# Dimensional variables
		if(request_names[v] == 'lat'){
			var_details[['Name']] = c('y','lat','latitude','Lat','Latitude')
			var_details[['UnitsName']] = c('degrees_north')
			var_details[['Multiplier']] = c(1)
			var_details[['Addition']] = c(0)
			var_details[['UnitsText']] = ''
			var_details[['PlotName']] = 'Latitude'
			var_details[['Range']] = c(-90,180)
		}else if(request_names[v] == 'lon'){
			var_details[['Name']] = c('x','lon','longitude','Lon','Longitude')
			var_details[['UnitsName']] = c('degrees_east')
			var_details[['Multiplier']] = c(1)
			var_details[['Addition']] = c(0)
			var_details[['UnitsText']] = ''
			var_details[['PlotName']] = 'Longitude'
			var_details[['Range']] = c(-180,360)
		# Met variables
		}else if(request_names[v] == 'SWdown'){
			var_details[['Name']] = c('SWdown')
			var_details[['UnitsName']] = c('W/m2','W/m^2','Wm^{-2}','wm2','watt/m^2')
			var_details[['Multiplier']] = c(1,1,1,1,1)
			var_details[['Addition']] = c(0,0,0,0,0)
			var_details[['UnitsText']] = 'W/'~m^{2}
			var_details[['PlotName']] = 'Downward shortwave radiation'
			var_details[['Range']] = c(0,1360)
		}else if(request_names[v] == 'LWdown'){
			var_details[['Name']] = c('LWdown')
			var_details[['UnitsName']] = c('W/m2','W/m^2','Wm^{-2}','wm2','watt/m^2')
			var_details[['Multiplier']] = c(1,1,1,1,1)
			var_details[['Addition']] = c(0,0,0,0,0)
			var_details[['UnitsText']] = 'W/'~m^{2}
			var_details[['PlotName']] = 'Downward longwave radiation'
			var_details[['Range']] = c(0,750)
		}else if(request_names[v] == 'Tair'){
			var_details[['Name']] = c('Tair')
			var_details[['UnitsName']] = c('K')
			var_details[['Multiplier']] = c(1)
			var_details[['Addition']] = c(0)
			var_details[['UnitsText']] = 'K'
			var_details[['PlotName']] = 'Surface air temperature'
			var_details[['Range']] = c(200,333)
		}else if(request_names[v] == 'Qair'){
			var_details[['Name']] = c('Qair')
			var_details[['UnitsName']] = c('kg/kg','g/g')
			var_details[['Multiplier']] = c(1,1)
			var_details[['Addition']] = c(0,0)
			var_details[['UnitsText']] = 'kg/kg'
			var_details[['PlotName']] = 'Specific humidity'
			var_details[['Range']] = c(0,0.1)
		}else if(request_names[v] == 'PSurf'){
			var_details[['Name']] = c('PSurf')
			var_details[['UnitsName']] = c('Pa','pa','hPa')
			var_details[['Multiplier']] = c(1,1,100) # when using first listed units
			var_details[['Addition']] = c(0,0,0)
			var_details[['UnitsText']] = 'Pa'
			var_details[['PlotName']] = 'Surface air pressure'
			var_details[['Range']] = c(50000,110000) # when using first listed units
		}else if(request_names[v] == 'Rainf'){
			var_details[['Name']] = c('Rainf')
			var_details[['UnitsName']] = c('mm/s','kg/m^2/s', 'kg/m^2s', 'mm/day')
			var_details[['Multiplier']] = c(1,1,1,86400)
			var_details[['Addition']] = c(0,0,0,0)
			var_details[['UnitsText']] = 'mm/s'
			var_details[['PlotName']] = 'Precipitation'
			var_details[['Range']] = c(0,0.05) # when using first listed units
		}else if(request_names[v] == 'Wind'){
			var_details[['Name']] = c('Wind')
			var_details[['UnitsName']] = c('m/s')
			var_details[['Multiplier']] = c(1)
			var_details[['Addition']] = c(0)
			var_details[['UnitsText']] = 'm/s'
			var_details[['PlotName']] = 'Windspeed'
			var_details[['Range']] = c(0,75)
		# Flux variables
		}else if(request_names[v] == 'Qh'){
			var_details[['Name']] = c('Qh','FSH') # FSH->CLM
			var_details[['UnitsName']] = c('W/m2','W/m^2','Wm^{-2}','wm2','watt/m^2')
			var_details[['Multiplier']] = c(1,1,1,1,1)
			var_details[['Addition']] = c(0,0,0,0,0)
			var_details[['UnitsText']] = 'W/'~m^{2}
			var_details[['PlotName']] = 'Sensible heat flux'
			var_details[['Range']] = c(-1000,1000)
		}else if(request_names[v] == 'Qle'){
			var_details[['Name']] = c('Qle','FCEV') # NB: FCEV is only PART of Qle for CLM
			var_details[['UnitsName']] = c('W/m2','W/m^2','Wm^{-2}','wm2','watt/m^2')
			var_details[['Multiplier']] = c(1,1,1,1,1)
			var_details[['Addition']] = c(0,0,0,0,0)
			var_details[['UnitsText']] = 'W/'~m^{2}
			var_details[['PlotName']] = 'Latent heat flux'
			var_details[['Range']] = c(-1000,1000)
		}else if(request_names[v] == 'Rnet'){
			var_details[['Name']] = c('Rnet')
			var_details[['UnitsName']] = c('W/m2','W/m^2','Wm^{-2}','wm2','watt/m^2')
			var_details[['Multiplier']] = c(1,1,1,1,1)
			var_details[['Addition']] = c(0,0,0,0,0)
			var_details[['UnitsText']] = 'W/'~m^{2}
			var_details[['PlotName']] = 'Net radiation'
			var_details[['Range']] =  c(-1000,1360)
		}else if(request_names[v] == 'SWnet'){
			var_details[['Name']] = c('SWnet')
			var_details[['UnitsName']] = c('W/m2','W/m^2','Wm^{-2}','wm2','watt/m^2')
			var_details[['Multiplier']] = c(1,1,1,1,1)
			var_details[['Addition']] = c(0,0,0,0,0)
			var_details[['UnitsText']] = 'W/'~m^{2}
			var_details[['PlotName']] = 'Net shortwave radiation'
			var_details[['Range']] =  c(0,1360)
		}else if(request_names[v] == 'LWnet'){
			var_details[['Name']] = c('LWnet')
			var_details[['UnitsName']] = c('W/m2','W/m^2','Wm^{-2}','wm2','watt/m^2')
			var_details[['Multiplier']] = c(1,1,1,1,1)
			var_details[['Addition']] = c(0,0,0,0,0)
			var_details[['UnitsText']] = 'W/'~m^{2}
			var_details[['PlotName']] = 'Net longwave radiation'
			var_details[['Range']] = c(0,750)
		}else if(request_names[v] == 'Qg'){
			var_details[['Name']] = c('Qg')
			var_details[['UnitsName']] = c('W/m2','W/m^2','Wm^{-2}','wm2','watt/m^2')
			var_details[['Multiplier']] = c(1,1,1,1,1)
			var_details[['Addition']] = c(0,0,0,0,0)
			var_details[['UnitsText']] = 'W/'~m^{2}
			var_details[['PlotName']] = 'Ground heat flux'
			var_details[['Range']] = c(-1000,1000)
		}else if(request_names[v] == 'NEE'){
			var_details[['Name']] = c('NEE','FCO2') # FCO2->CLM
			var_details[['UnitsName']] = c('umol/m2/s','mumol/m2/s','umol/m^2/s',
				'umol/m2s','gC/m^2/s','gC/m2/s')
			var_details[['Multiplier']] = c(1,1,1,1,1/(1.201E-5),1/(1.201E-5))
			var_details[['Addition']] = c(0,0,0,0,0)
			var_details[['UnitsText']] = ~mu~'mol/'~m^{2}~'/s' # default PALS units for plots
			var_details[['PlotName']] = 'Net ecosystem exchange'
			var_details[['Range']] = c(-100,100)
		}else if(request_names[v] == 'GPP'){
			var_details[['Name']] = c('GPP')
			var_details[['UnitsName']] = c('umol/m2/s','mumol/m2/s','umol/m^2/s',
				'umol/m2s','gC/m^2/s','gC/m2/s')
			var_details[['Multiplier']] = c(1,1,1,1,1/(1.201E-5),1/(1.201E-5))
			var_details[['Addition']] = c(0,0,0,0,0)
			var_details[['UnitsText']] = ~mu~'mol/'~m^{2}~'/s' # default PALS units for plots
			var_details[['PlotName']] = 'Gross primary production'
			var_details[['Range']] = c(-100,100)
		}
		variable_list[[v]] = var_details
	}
	return(variable_list)
}
GetVariableIndex = function(vars,varname){
	# Returns the index of a requested variable in a
	# vars variable from a call to GetVariableDetails
	vindex=NA
	for(v in 1:length(vars)){
		if(vars[[v]][['Name']][1] == varname){
			vindex = v
			break
		}
	}
	return(vindex)	
}