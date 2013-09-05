# Timing.R
#
# Functions that assess or convert timing variables.
#
# Gab Abramowitz UNSW 2012 (palshelp at gmail dot com)
#
CheckSpreadsheetTiming = function(DataFromText) {
	# Checks that uploaded spreadsheet data is compatible 
	# with time step size in web form; that a whole number of 
	# days are present; and whether there are an integer 
	# number of years.
	tstepinday=86400/DataFromText$timestepsize # time steps in a day
	ndays = DataFromText$ntsteps/tstepinday # number of days in data set
	if((ndays - round(ndays)) != 0){
		CheckError(paste('S2: Spreadsheet does not appear to contain a',
			'whole number of days of data. Please amend.'))
	}
	if((DataFromText$starttime$sday != 1) | (DataFromText$starttime$smonth != 1)){
		CheckError(paste('S2: Spreadsheet data does not appear to begin',
			'on 1st January. Please amend.'))
	}
}
CreateTimeunits = function(starttime) {
	# Determine data start date and time:
	shour = floor(starttime$shod)
	smin = floor((starttime$shod - shour)*60)
	ssec = floor(((starttime$shod - shour)*60 - smin)*60)
	start_hod = paste(Create2Uchar(shour),':',Create2Uchar(smin),':',Create2Uchar(ssec),sep='')
	timeunits=paste('seconds since ',as.character(starttime$syear),'-',
		Create2Uchar(starttime$smonth),'-',Create2Uchar(starttime$sday),' ',
		start_hod,sep='')
	return(timeunits)
}
Yeardays = function(startyear,ndays) {
	# Returns: an integer vector of possible number of days in a 
	# dataset containing a whole number of years; whet
	if(ndays<365){
		CheckError(paste('S2: Dataset appears to have less than 1 year of data. Days:',
			ndays,'Startyear:',startyear))
	}
	daysperyear = c()
	ctr=1 # initialise
	year = startyear # initialise
	days=ndays # initialise
	lpyrs = 0
	# Incrementally remove year of days from total number of days:
	repeat {
		ctr = ctr + 1
		if(is.leap(year)){	
			days = days - 366
			daysperyear[ctr] = 366
			lpyrs = lpyrs + 1
		}else{
			days = days - 365
			daysperyear[ctr] = 365
		}
		year = year + 1
		if(days<365){
			if(days>0 && days!=(365-lpyrs)){ # ie. after removing whole years, days are left over
				daysperyear[ctr+1] = days
				whole=FALSE
			}else if(days==(365-lpyrs)){ # i.e. non leap year data set
				daysperyear[ctr+1] = days
				whole=TRUE
			}else{ # =0
				whole=TRUE
			}
			break
		}
	}
	# Create return list:
	yeardays = list(daysperyear=daysperyear,whole=whole)
	return(yeardays)
}
is.leap = function(year){
	if((((year %% 4)==0) & ((year %% 100)!=0)) || 
		(((year %% 4)==0) & ((year %% 400)==0))){
		leap=TRUE	
	}else{
		leap=FALSE
	}
	return(leap)
}
GetTimingNcfile = function(fid){
	# This function gets the time step size, number of timesteps
	# and start date and time details from a netcdf file.
	# First, find the model time step size:
	exists = FALSE # initialise
	tsize = NA; ntsteps = NA; syear = NA; smonth = NA; sdoy = NA
	nvars = length(fid$var) # number of variables in netcdf file
	ndims = length(fid$dim) # number of dimensions in netcdf file
	for (v in 1:nvars){ # Search through all variables in netcdf file
		if(substr(fid$var[[v]]$name,1,6)=='t_ave_'){ # i.e. ORCHIDEE file
			exists = TRUE
			# Read 'time' variable:
			time12=get.var.ncdf(fid,fid$var[[v]]$name,start=1,count=2) 
			tsize=time12[2]-time12[1]
		}else if(substr(fid$var[[v]]$name,1,5)=='mscur'){ # i.e. CLM file
			exists = TRUE
			# Read 'time' variable:
			time12=get.var.ncdf(fid,fid$var[[v]]$name,start=1,count=2) 
			tsize=time12[2]-time12[1]
			# Read date variable:
			date1=as.character(get.var.ncdf(fid,'mcdate',start=1,count=1))
			syear = as.numeric(substr(date1,1,4))
			smonth = as.numeric(substr(date1,5,6))
			sdoy = as.numeric(substr(date1,7,8))
		}
	}
	if(!exists){ # i.e. none of the above time variables were found
		# Search for time as a dimension variable:
		for (d in 1:ndims){ # Search through all dimensions in netcdf file
			if(fid$dim[[d]]$name=='time' | fid$dim[[d]]$name=='t' |
				fid$dim[[d]]$name=='time_counter'){
				# Now check for time dimension variable:
				for (v in 1:nvars){ # Search through all variables in netcdf file
					if(fid$var[[v]]$name=='time'){
						exists = TRUE # i.e. found time dimension variable
						# Get time units:
						timeunits = att.get.ncdf(fid,'time','units')
						break
					}
				}
				if(!exists){ # read all non-dimvars and had no luck
					exists = TRUE # i.e. found time dimension variable
					# Assume dim variable
					timeunits = att.get.ncdf(fid,'time','units')
				}
				break	
			}
		}
		if(exists){
			if(substr(timeunits$value,1,4)=='seco'){ # time units are seconds
				time12=get.var.ncdf(fid,'time',start=1,count=2) 
				tsize=time12[2]-time12[1] # set time step size
				if( !(tsize>=300 && tsize<=3600) ){
					CheckError(paste('T1: Unable to ascertain time step size in',
						stripFilename(fid$filename)))
				}
				syear = as.numeric(substr(timeunits$value,15,18))
				smonth = as.numeric(substr(timeunits$value,20,21))
				sdoy = as.numeric(substr(timeunits$value,23,24))
			}
		}else{# i.e. still haven't found timing variable
			# Return to parent function with error:
			CheckError(paste('T1: Unable to ascertain time step size in',
				stripFilename(fid$filename)))
		}
	}
	# Find out how many time steps there are in the model file:
	for (d in 1:ndims){ # Search through all dimensions in netcdf file
		if(fid$dim[[d]]$unlim){ # assume this is the timestep dimension
			# Store number of time steps in this model file:
			ntsteps = fid$dim[[d]]$len
			break # stop searching for unlim dim
		}
	}
	if(!(ntsteps>=12 && ntsteps < 1e9)){
		CheckError(paste('T1: Unable to determine number of time steps in file:',
			stripFilename(fid$filename)))
	}
	tstepinday=86400/tsize # time steps in a day
	ndays = ntsteps/tstepinday # number of days in data set
	intyear = Yeardays(syear,ndays)
	# Create return list:
	timing = list(tstepsize=tsize,exists=exists,tsteps=ntsteps,
		syear=syear,smonth=smonth,sdoy=sdoy,whole=intyear$whole)
	return(timing)
}
getMonthDays = function(leap=FALSE) {
	# The days on which each month begins:
	month_start=c()
	month_length=c()
	month_start[1]=1    # Jan
	month_start[2]=32   # Feb
	if(leap){
		month_start[3]=61   # Mar
		month_start[4]=92   # Apr
		month_start[5]=122  # May
		month_start[6]=153  # Jun
		month_start[7]=183  # Jul
		month_start[8]=214  # Aug
		month_start[9]=245  # Sep
		month_start[10]=275 # Oct
		month_start[11]=306 # Nov
		month_start[12]=336 # Dec
		month_start[13]=367 # i.e. beginning of next year
	}else{
		month_start[3]=60   # Mar
		month_start[4]=91   # Apr
		month_start[5]=121  # May
		month_start[6]=152  # Jun
		month_start[7]=182  # Jul
		month_start[8]=213  # Aug
		month_start[9]=244  # Sep
		month_start[10]=274 # Oct
		month_start[11]=305 # Nov
		month_start[12]=335 # Dec
		month_start[13]=366 # i.e. beginning of next year
	}
	for(m in 1:12){
		month_length[m]=month_start[m+1]-month_start[m]
	}
	month = list(start=month_start,length=month_length)
	return(month)
}
# doydate returns the day and month, given day of year:
doydate = function(doy,leap=FALSE){
	month=getMonthDays(leap)
	# Find month of this doy
	for(m in 1:12){
		if(doy >= month$start[m] && doy < month$start[m+1]){
			doymonth = m
			doyday = doy - month$start[m] + 1
		}
	}
	date = list(month = doymonth, day = doyday)
	return(date)
}
# Provides time and date for a given time step:
dateFromTstep = function(starttime,tstep){
	ntstepsinday = (1 / starttime$timestepsize) * 24 # assume in hours
	nowtstep = tstep # initialise current tstep
	nowday = 1 + tstep / ntstepsinday # initialise number of days
	nowyear = starttime$year # initialise year
	repeat { # for each year that has passed before this tstep
		mdays = getMonthDays(is.leap(nowyear)) # get ndays in year
		if(nowday >= mdays$start[13]){ # number of days in year
			nowtstep = nowtstep - mdays$start[13]*ntstepsinday # reduce remaining
			nowday = nowday - mdays$start[13] + 1 # reduce remaining days
			nowyear = nowyear + 1 # increment year
		}else{ # # outstanding days < a year
			realdate = doydate(nowday,is.leap(nowyear))
			realtime = (tstep %% ntstepsinday) * starttime$timestepsize
			break
		}
	}
	nowdate = list(time = realtime, day = as.integer(realdate$day), 
		month = realdate$month, year = nowyear)
	return(nowdate)
	
}

Create2Uchar = function(intin){
	# Creates string of length 2 from integer of length 1 or 2
	if(intin<10){
		temp=as.character(intin)
		charout=paste('0',temp,sep='')
	}else if(intin>99){
		charout='NA'
		CheckError('I3: Character variable too long in function Create2Uchar.')
	}else{
		charout=as.character(intin)	
	}
	return(charout)
}
DayNight = function(SWdown,threshold = 5){
	# Returns a logical day/night time series based on a SW threshold:
	daynotnight = c()
	daynotnight[1:length(SWdown)] = FALSE
	for(t in 1:length(SWdown)){
		if(SWdown[t]>threshold){
			daynotnight[t]=TRUE
		}
	}
	return(daynotnight)
}