# PALSchecks.R
#
# Gab Abramowitz UNSW 2012 (palshelp at gmail dot com)
#
# Finds variable value that is outside acceptable ranges.
FindRangeViolation = function(varin,varrange){
	offendingValue=0 # init
	for(i in 1:length(varin)){
		if(varin[i]<varrange[1] | varin[i]>varrange[2]){
			offendingValue = varin[i]
			return(offendingValue)
		}
	}
	return(offendingValue) 
}

CheckTiming = function(timing1,timing2,benchmark_timing=FALSE){
	# Simply checks whether model and obs (and maybe benchmark) 
	# time step size and number of time steps are compatible.
	errtext = 'ok'
	err = FALSE
	if(timing1$tstepsize != timing2$tstepsize){
		if(benchmark_timing){
			errtext = paste('B2: Time step size differs between',
				'observed data set and benchmark time series:',
				timing1$tstepsize, timing2$tstepsize)
			err=TRUE
		}else{
			errtext = paste('M1: Time step size differs between',
				'observed data set and model output:',
				timing1$tstepsize, timing2$tstepsize)
			err=TRUE
		}
	}else if(timing1$tsteps != timing2$tsteps){
		if(benchmark_timing){
			errtext = paste('B2: Number of time steps differs between',
				'observed data set and benchmark time series:',
				timing1$tsteps, timing2$tsteps)
			err=TRUE
		}else{
			errtext = paste('M1: Number of time steps differs between',
				'observed data set and model output:',
				timing1$tsteps, timing2$tsteps)
			err=TRUE
		}
	}
	result = list(err=err,errtext=errtext)
	return(result)
}

CheckVersionCompatibility = function(filepath1,filepath2){
	# Given tow netcdf files produced by PALS, checks that
	# they're produced using the same dataset name and version.
	fid1=open.ncdf(filepath1,readunlim=FALSE) # open file 1
	fid2=open.ncdf(filepath2,readunlim=FALSE) # open file 2
	# Get PALS data set name and version for both files:
	DsetName1 = att.get.ncdf(fid1,varid=0,attname='PALS_dataset_name')
	DsetName2 = att.get.ncdf(fid2,varid=0,attname='PALS_dataset_name')
	DsetVer1 = att.get.ncdf(fid1,varid=0,attname='PALS_dataset_version')
	DsetVer2 = att.get.ncdf(fid2,varid=0,attname='PALS_dataset_version')
	if(tolower(DsetName1$value) != tolower(DsetName2$value)){
		#CheckError(paste('B3: Data set name in observed data',
		#	'file and benchmark file is different:',
		#	DsetName1$value,DsetName2$value))
	}
	if(tolower(DsetVer1$value) != tolower(DsetVer2$value)){
		#CheckError(paste('B3: Data set version in observed data',
		#	'file and benchmark file is different:',
		#	DsetVer1$value,DsetVer2$value))
	}
}

CheckTextDataRanges = function(datain,found){
	# Get acceptable ranges for variables:	
	range = GetVariableRanges()
	# Check variable ranges:
	if(any(datain$data$SWdown<range$SWdown[1])|
		any(datain$data$SWdown>range$SWdown[2])){
		badval = FindRangeViolation(datain$data$SWdown,range$SWdown)
		errtext = paste('S2: Downward SW radiation outside expected',
			' ranges: ',as.character(badval),' [',
			as.character(range$SWdown[1]),':',
			as.character(range$SWdown[2]),']',sep='')
		CheckError(errtext)
	}
	if(found$LWdown){
		if(any(datain$data$LWdown<range$LWdown[1])|
			any(datain$data$LWdown>range$LWdown[2])){
			badval = FindRangeViolation(datain$data$LWdown,range$LWdown)
			errtext = paste('S2: Downward LW radiation outside expected',
				' ranges: ',as.character(badval),' [',
				as.character(range$LWdown[1]),':',
				as.character(range$LWdown[2]),']',sep='')
			CheckError(errtext)
		}
	}
	if(any(datain$data$Tair<range$Tair[1])|
		any(datain$data$Tair>range$Tair[2])){
		badval = FindRangeViolation(datain$data$Tair,range$Tair)
		errtext = paste('S2: Surface air temperature outside expected',
			' ranges: ',as.character(badval),' [',
			as.character(range$Tair[1]),':',
			as.character(range$Tair[2]),']',sep='')
		CheckError(errtext)
	}
	if(any(datain$data$Qair<range$Qair[1])|
		any(datain$data$Qair>range$Qair[2])){
		badval = FindRangeViolation(datain$data$Qair,range$Qair)
		errtext = paste('S2: Specific humidity outside expected',
			' ranges: ',as.character(badval),' [',
			as.character(range$Qair[1]),':',
			as.character(range$Qair[2]),']',sep='')
		CheckError(errtext)
	}
	if(any(datain$data$Wind<range$Wind[1])|
		any(datain$data$Wind>range$Wind[2])){
		badval = FindRangeViolation(datain$data$Wind,range$Wind)
		errtext = paste('S2: Scalar windspeed outside expected',
			' ranges: ',as.character(badval),' [',
			as.character(range$Wind[1]),':',
			as.character(range$Wind[2]),']',sep='')
		CheckError(errtext)
	}
	if(any(datain$data$Rainf<range$Rainf[1])|
		any(datain$data$Rainf>range$Rainf[2])){
		badval = FindRangeViolation(datain$data$Rainf,range$Rainf)
		errtext = paste('S2: Rainfall rate outside expected',
			' ranges: ',as.character(badval),' [',
			as.character(range$Rainf[1]),':',
			as.character(range$Rainf[2]),']',sep='')
		CheckError(errtext)
	}
	if(found$Snowf){
		if(any(datain$data$Snowf<range$Snowf[1])|
			any(datain$data$Snowf>range$Snowf[2])){
			badval = FindRangeViolation(datain$data$Snowf,range$Snowf)
			errtext = paste('S2: Snowfall rate outside expected',
				' ranges: ',as.character(badval),' [',
				as.character(range$Snowf[1]),':',
				as.character(range$Snowf[2]),']',sep='')
			CheckError(errtext)
		}
	}
	if(any(datain$data$PSurf<range$PSurf[1])|
		any(datain$data$PSurf>range$PSurf[2])){
		badval = FindRangeViolation(datain$data$PSurf,range$PSurf)
		errtext = paste('S2: Surface air pressure outside expected',
			' ranges: ',as.character(badval),' [',
			as.character(range$PSurf[1]),':',
			as.character(range$PSurf[2]),']',sep='')
		CheckError(errtext)
	}
	if(any(datain$data$Qle<range$Qle[1])|
		any(datain$data$Qle>range$Qle[2])){
		badval = FindRangeViolation(datain$data$Qle,range$Qle)
		errtext = paste('S2: Latent heat flux outside expected',
			' ranges: ',as.character(badval),' [',
			as.character(range$Qle[1]),':',
			as.character(range$Qle[2]),']',sep='')
		CheckError(errtext)
	}
	if(any(datain$data$Qh<range$Qh[1])|
		any(datain$data$Qh>range$Qh[2])){
		badval = FindRangeViolation(datain$data$Qh,range$Qh)
		errtext = paste('S2: Sensible heat flux outside expected',
			' ranges: ',as.character(badval),' [',
			as.character(range$Qh[1]),':',
			as.character(range$Qh[2]),']',sep='')
		CheckError(errtext)
	}
	if(any(datain$data$NEE<range$NEE[1])|
		any(datain$data$NEE>range$NEE[2])){
		badval = FindRangeViolation(datain$data$NEE,range$NEE)
		errtext = paste('S2: Net ecosystem exchange outside expected',
			' ranges: ',as.character(badval),' [',
			as.character(range$NEE[1]),':',
			as.character(range$NEE[2]),']',sep='')
		CheckError(errtext)
	}
}

# Check the existence of optional variables:
CheckTextDataVars = function(datain){
	# First check that all essential variables are present:
	if(any(datain$data$SWdown==SprdMissingVal)){
		CheckError('S2: Downward shortwave has missing values.')
	}
	if(any(datain$data$Tair==SprdMissingVal)){
		CheckError('S2: Air temperature has missing values.')
	}
	if(any(datain$data$Qair==SprdMissingVal)){
		CheckError('S2: Humidity has missing values.')
	}
	if(any(datain$data$Wind==SprdMissingVal)){
		CheckError('S2: Windspeed has missing values.')
	}
	if(any(datain$data$Rainf==SprdMissingVal)){
		CheckError('S2: Rainfall has missing values.')
	}
	# Initialise list of found variables:
	LWdown = FALSE# surface incident longwave rad [W/m^2]
	LWdown_all = FALSE# gapless surface incident longwave rad [W/m^2]
	Snowf = FALSE # snowfall rate [mm/s]
	PSurf = FALSE # surface air pressure [Pa]
	CO2air = FALSE# near surface CO2 concentration [ppmv]
	Qle = FALSE   # latent heat flux [W/m^2]
	Qh = FALSE    # sensible heat flux [W/m^2]
	Qg = FALSE    # ground heat flux [W/m^2]
	NEE = FALSE   # net ecosystem exchange CO2 [umol/m^2/s]
	GPP = FALSE   # gross primary production CO2 [umol/m^2/s]
	SWup = FALSE  # reflected SW rad [W/m^2]
	Rnet = FALSE  # net absorbed radiation [W/m^2]
	SWdown_qc = FALSE
	Tair_qc = FALSE
	Qair_qc = FALSE
	Wind_qc = FALSE
	Rainf_qc = FALSE
	LWdown_qc = FALSE
	Snowf_qc = FALSE 
	PSurf_qc = FALSE 
	CO2air_qc = FALSE
	Qle_qc = FALSE   
	Qh_qc = FALSE    
	Qg_qc = FALSE    
	NEE_qc = FALSE
	GPP_qc = FALSE
	SWup_qc = FALSE  
	Rnet_qc = FALSE  
	found = list(LWdown=LWdown,LWdown_all=LWdown_all,Snowf=Snowf,
		PSurf=PSurf,CO2air=CO2air,Qle=Qle,Qh=Qh,Qg=Qg,NEE=NEE,
		SWup=SWup,Rnet=Rnet,SWdown_qc=SWdown_qc,Tair_qc=Tair_qc,
		Qair_qc=Qair_qc,Wind_qc=Wind_qc,Rainf_qc=Rainf_qc,
		LWdown_qc=LWdown_qc,Snowf_qc=Snowf_qc,PSurf_qc=PSurf_qc,
		CO2air_qc=CO2air_qc,Qle_qc=Qle_qc,Qh_qc=Qh_qc,
		Qg_qc=Qg_qc,NEE_qc=NEE_qc,GPP_qc=GPP_qc,SWup_qc=SWup_qc,
		Rnet_qc=Rnet_qc)
	
	# Begin checking:
	if(any(datain$data$LWdown!=SprdMissingVal)){ # note unusual condition
		found$LWdown = TRUE # some data present
		if(!any(datain$data$LWdown==SprdMissingVal)){
			found$LWdown_all = TRUE # all data present
		}
	}
	if(!any(datain$data$Snowf==SprdMissingVal)){
		found$Snowf = TRUE
	}
	if(!any(datain$data$PSurf==SprdMissingVal)){
		found$PSurf = TRUE
	}
	if(!any(datain$data$CO2air==SprdMissingVal)){
		found$CO2air = TRUE
	}
	if(!any(datain$data$Qle==SprdMissingVal)){
		found$Qle = TRUE
	}
	if(!any(datain$data$Qh==SprdMissingVal)){
		found$Qh = TRUE
	}
	if(!any(datain$data$Qg==SprdMissingVal)){
		found$Qg = TRUE
	}
	if(!any(datain$data$NEE==SprdMissingVal)){
		found$NEE = TRUE
	}
	if(!any(datain$data$GPP==SprdMissingVal) && 
		datain$templateVersion!='1.0.1'){
		found$GPP = TRUE
	}
	if(!any(datain$data$SWup==SprdMissingVal)){
		found$SWup = TRUE
	}
	if(!any(datain$data$Rnet==SprdMissingVal)){
		found$Rnet = TRUE
	}
	# Note change of "found" criteria for qc flags:
	if(any(datain$data$SWdownFlag!=SprdMissingVal)){
		found$SWdown_qc = TRUE
	}
	if(any(datain$data$TairFlag!=SprdMissingVal)){
		found$Tair_qc = TRUE
	}
	if(any(datain$data$QairFlag!=SprdMissingVal)){
		found$Qair_qc = TRUE
	}
	if(any(datain$data$RainfFlag!=SprdMissingVal)){
		found$Rainf_qc = TRUE
	}
	if(any(datain$data$LWdownFlag!=SprdMissingVal)){
		found$LWdown_qc = TRUE
	}
	if(any(datain$data$SnowfFlag!=SprdMissingVal)){
		found$Snowf_qc = TRUE
	}
	if(any(datain$data$PSurfFlag!=SprdMissingVal)){
		found$PSurf_qc = TRUE
	}else if(! found$PSurf){
		# If we didn't find PSurf, we know PALS will 
		# synthesize, and we'll mark that in qc variable.
		found$PSurf_qc = TRUE
	}
	if(any(datain$data$CO2airFlag!=SprdMissingVal)){
		found$CO2air_qc = TRUE
	}
	if(any(datain$data$WindFlag!=SprdMissingVal)){
		found$Wind_qc = TRUE
	}
	if(any(datain$data$QleFlag!=SprdMissingVal)){
		found$Qle_qc = TRUE
	}
	if(any(datain$data$QhFlag!=SprdMissingVal)){
		found$Qh_qc = TRUE
	}
	if(any(datain$data$QgFlag!=SprdMissingVal)){
		found$Qg_qc = TRUE
	}
	if(any(datain$data$NEEFlag!=SprdMissingVal)){
		found$NEE_qc = TRUE
	}
	if(any(datain$data$GPPFlag!=SprdMissingVal)){
		found$GPP_qc = TRUE
	}
	if(any(datain$data$SWupFlag!=SprdMissingVal)){
		found$SWup_qc = TRUE
	}
	if(any(datain$data$RnetFlag!=SprdMissingVal)){
		found$Rnet_qc = TRUE
	}
	# IF no LSM testing variables are found, report it:
	if((!found$Qle)&(!found$Qh)&(!found$NEE)&(!found$Rnet)&
		(!found$GPP)&(!found$SWup)&(!found$Qg)){
		CheckError(paste('S2: Could not find any LSM evaluation',
		'varaibles: Qle, Qh, Qg, NEE, GPP, SWup or Rnet.'))
	}
	return(found)	
}

