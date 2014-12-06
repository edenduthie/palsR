# GetGLEAM_Aus.R
#
# This script fetches GLEAM ET data masked for Australia
#
# Gab Abramowitz, UNSW, 2013, gabsun at gmail dot com
#
# Inputs:
# missing_threshold = what percentage of missing data in a grid
# 					cell is acceptable?
#
# Default values are those specified in function arguments:

GetGLEAM_Aus = function(variable,filelist,force_interval='no',dsetversion='default'){
	library(ncdf4) # load package
	# Should really access complete GLEAM data files and pull out Aus,
	# but for now...
	errtext='ok'	
	if((variable[['Name']][1] != 'Qle') && (variable[['Name']][1] != 'Evap')){
		errtext = 'Request for non-Qle, non-Evap variable to GLEAM_Aus read routine.'
		obs = list(err=TRUE,errtext=errtext)
		return(obs)
	}
	nyears = length(filelist)
	year = c() 
	for(f in 1:nyears){ # For each file sent by js
		# Establish which year the file contains:
		year[f] = as.numeric(substr(filelist[[f]][['path']], 
			(nchar(filelist[[f]][['path']])-6), (nchar(filelist[[f]][['path']])-3) ) )
	}
	# Define the order to read files:
	fileorder = order(year)	
	# Define number of days in total:
	yds = Yeardays(min(year),nyears*366)
	daysvector = yds$daysperyear
	ndays = sum(daysvector[1:nyears])
	dayctr = 1 # initialise
	if((force_interval == 'no') | (force_interval == 'daily')){
		interval = 'daily'
		tsteps = ndays
		ET = array(NA,dim=c(208,143,ndays))	# Initialise data array:
	}else if(force_interval == 'monthly'){
		interval = 'monthly'
		tsteps = nyears*12
		ET = array(NA,dim=c(208,143,tsteps))	# Initialise data array:
	}else{
		errtext = paste('GLEAM_Aus requested to force to unknown interval:',force_interval)
		obs = list(err=TRUE,errtext=errtext)
		return(obs)	
	}
	# Get data:
	for(f in 1:nyears){ # For each file sent by js		
		# Open file:
		fid = nc_open(filelist[[ fileorder[f] ]][['path']],write=FALSE,readunlim=FALSE)
		# Read GLEAM data for this year:
		if(interval == 'daily'){
			ET[,,dayctr:(dayctr + daysvector[f] - 1)] = ncvar_get(fid, 'EVAP' ) # read model output data
		}else if(interval == 'monthly'){
			tmp = ncvar_get(fid, 'EVAP' ) # read model output data
			ET[,, ((f-1)*12+1) : ((f-1)*12+12)] = DailyToMonthly(tmp,year[fileorder[f]],daysvector[f])
		}	
		# Increment counter:
		dayctr = dayctr + daysvector[f]
		# Close netcdf file for this year:
		nc_close(fid)
	}
	# Reopen first file to fetch lat and lon:
	fid = nc_open(filelist[[ 1 ]][['path']],write=FALSE,readunlim=FALSE)
	# Fetch lat and lon:
	lat=ncvar_get(fid, 'lat' ) # read latitude 
	lon=ncvar_get(fid, 'lon' ) # read latitude 
	# Close netcdf file :
	nc_close(fid)
	
	if(variable[['Name']][1] == 'Qle'){
		ET = ET*28.4 # convert from mm/day to W/m^2
	}
	
	timing = list(interval=interval,tsteps=tsteps)
	
	# Return result
	obs = list(err=FALSE,errtext=errtext,data=ET,lat=lat,lon=lon,timing=timing,name='GLEAM')
	return(obs)	
}