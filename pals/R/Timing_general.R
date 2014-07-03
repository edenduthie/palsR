# Timing_general.R
#
# Functions that assess or convert timing variables.
#
# Gab Abramowitz UNSW 2014 (palshelp at gmail dot com)
#

Yeardays = function(startyear,ndays) {
	# Returns: an integer vector of possible number of days in a 
	# dataset containing a whole number of years; whet
	if(ndays<365){
		whole=FALSE
		daysperyear = ndays
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

doydate = function(doy,leap=FALSE){
	# Doydate returns the day of month and month, given day of year:
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