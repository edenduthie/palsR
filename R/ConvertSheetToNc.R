# SpreadsheetToNc.R
#
# A collection of functions to convert flux tower
# data from spreadsheet to netcdf.
#
# Gab Abramowitz UNSW 2012 (palshelp at gmail dot com)
#
ChangeMetUnits = function(datain,found,elevation){
	# Performs units changes from flux data provider
	# template units to netcdf met/flux file units.
	# First calculate timestep size:
	timestepsize = 
		(datain$data$LocHoD[2] - datain$data$LocHoD[1])*3600
	# Temperature from C to K:
	datain$data$Tair = datain$data$Tair + zeroC
	if(found$PSurf){
		# Pressure from mbar to Pa
		datain$data$PSurf = datain$data$PSurf * 100
	}else{
		# Synthesize PSurf based on temperature and elevation
		datain$data$PSurf = 101325 * (datain$data$Tair/
			(datain$data$Tair + 0.0065*elevation))^(9.80665/287.04/0.0065)
		datain$data$PSurfFlag = 0 # i.e. all gap-filled
	}
	# Rainfall from mm/timestep to mm/s
	datain$data$Rainf = datain$data$Rainf/timestepsize
	if(found$Snowf){
		# Snowfall from mm/timestep to mm/s
		datain$data$Snowf = datain$data$Snowf/timestepsize
	}
	# Relative to specific humidity:
	datain$data$Qair = Rel2SpecHum(datain$data$Qair,
		datain$data$Tair,datain$data$PSurf)
	return(datain)
}
CreateFluxNcFile = function(fluxfilename,datain,latitude,longitude,
	timestepsize,datasetname,datasetversion,found,starttime,templateVersion,
	elevation=NA,measurementheight=NA,canopyheight=NA,
	vegetationtype=NA,utcoffset=NA,avprecip=NA,avtemp=NA){
	# This function, sent observed flux variables uploaded by a user
	# in text format, creates a netcdf file which is downloadable for
	# the flux data provider and is used internally for analysis scripts.
	library(ncdf) # load netcdf library
	missing_value=NcMissingVal # default missing value for all variables
	# Define x, y and z dimensions
	xd = dim.def.ncdf('x',vals=c(1),units='')	
	yd = dim.def.ncdf('y',vals=c(1),units='')
	# Determine data start date and time:
	timeunits = CreateTimeunits(starttime)
	# Create time dimension variable:
	tt=c(0:(datain$ntsteps-1))
	timedata = as.double(tt*timestepsize)
	# Define time dimension:
	td = dim.def.ncdf('time',unlim=TRUE,units=timeunits,vals=timedata)
	# VARIABLE DEFINITIONS ##############################################
	# First, non-time variables:
	# Define latitude:
	lat=var.def.ncdf('latitude','degrees_north',dim=list(xd,yd),
		missval=missing_value,longname='Latitude')
	# Define longitude:
	lon=var.def.ncdf('longitude','degrees_east',dim=list(xd,yd),
		missval=missing_value,longname='Longitude')
	
	# Initialise variable list:
	fluxncvars = list(lat,lon)
	ctr = 3
	if(found$Qle){ # Define Qle variable:
		Qle=var.def.ncdf('Qle','W/m^2', dim=list(xd,yd,td),
			missval=missing_value,longname='Latent heat flux from surface')
		fluxncvars[[ctr]] = Qle
		ctr = ctr + 1
	}
	if(found$Qh){ # Define Qh variable:
		Qh=var.def.ncdf('Qh','W/m^2', dim=list(xd,yd,td),
			missval=missing_value,longname='Sensible heat flux from surface')
		fluxncvars[[ctr]] = Qh
		ctr = ctr + 1
	}
	if(found$NEE){ # Define NEE variable:
		NEE=var.def.ncdf('NEE','umol/m^2/s', dim=list(xd,yd,td),
			missval=missing_value,longname='Net ecosystem exchange of CO2')
		fluxncvars[[ctr]] = NEE
		ctr = ctr + 1
	}
	if(found$GPP){ # Define GPP variable:
		GPP=var.def.ncdf('GPP','umol/m^2/s', dim=list(xd,yd,td),
			missval=missing_value,longname='Gross primary poductivity of CO2')
		fluxncvars[[ctr]] = GPP
		ctr = ctr + 1
	}
	if(found$Qg){ # Define Qg variable:
		Qg=var.def.ncdf('Qg','W/m^2', dim=list(xd,yd,td),
			missval=missing_value,longname='Ground heat flux')
		fluxncvars[[ctr]] = Qg
		ctr = ctr + 1
	}
	if(found$SWup){ # Define SWup and SWnet variables:
		SWup=var.def.ncdf('SWup','W/m^2', dim=list(xd,yd,td),
			missval=missing_value,longname='Reflected shortwave radiation')
		fluxncvars[[ctr]] = SWup
		ctr = ctr + 1
		SWnet=var.def.ncdf('SWnet','W/m^2', dim=list(xd,yd,td),
			missval=missing_value,longname='Net absorbed shortwave radiation')
		fluxncvars[[ctr]] = SWnet
		ctr = ctr + 1
	}
	if(found$Rnet){ # Define Rnet variable:
		Rnet=var.def.ncdf('Rnet','W/m^2', dim=list(xd,yd,td),
			missval=missing_value,longname='Net absorbed radiation')
		fluxncvars[[ctr]] = Rnet
		ctr = ctr + 1
	}
	# Define quality control flag variables:
	if(found$Qle_qc){ # Define Qle_qc variable:
		Qle_qc=var.def.ncdf('Qle_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='Qle quality control flag')
		fluxncvars[[ctr]] = Qle_qc
		ctr = ctr + 1
	}
	if(found$Qh_qc){ # Define Qh_qc variable:
		Qh_qc=var.def.ncdf('Qh_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='Qh quality control flag')
		fluxncvars[[ctr]] = Qh_qc
		ctr = ctr + 1
	}
	if(found$NEE_qc){ # Define NEE_qc variable:
		NEE_qc=var.def.ncdf('NEE_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='NEE quality control flag')
		fluxncvars[[ctr]] = NEE_qc
		ctr = ctr + 1
	}
	if(found$GPP_qc){ # Define GPP_qc variable:
		GPP_qc=var.def.ncdf('GPP_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='GPP quality control flag')
		fluxncvars[[ctr]] = GPP_qc
		ctr = ctr + 1
	}
	if(found$Qg_qc){ # Define Qg_qc variable:
		Qg_qc=var.def.ncdf('Qg_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='Qg quality control flag')
		fluxncvars[[ctr]] = Qg_qc
		ctr = ctr + 1
	}
	if(found$SWup_qc){ # Define SWup_qc and SWnet_qc variables:
		SWup_qc=var.def.ncdf('SWup_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='SWup quality control flag')
		fluxncvars[[ctr]] = SWup_qc
		ctr = ctr + 1
		SWnet_qc=var.def.ncdf('SWnet_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='SWnet quality control flag')
		fluxncvars[[ctr]] = SWnet_qc
		ctr = ctr + 1
	}
	if(found$Rnet_qc){ # Define Rnet_qc variable:
		Rnet_qc=var.def.ncdf('Rnet_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='Rnet quality control flag')
		fluxncvars[[ctr]] = Rnet_qc
		ctr = ctr + 1
	}
	# Define elevation:
	if(!is.na(elevation)){
		elev=var.def.ncdf('elevation','m',dim=list(xd,yd),
			missval=missing_value,longname='Site elevation above sea level')
		fluxncvars[[ctr]] = elev
		ctr = ctr + 1
	}
	# Define measurement height on tower:
	if(!is.na(measurementheight)){
		refheight=var.def.ncdf('reference_height','m',dim=list(xd,yd),
			missval=missing_value,longname='Measurement height on flux tower')
		fluxncvars[[ctr]] = refheight
		ctr = ctr + 1
	}
	# Define canopy height:
	if(!is.na(canopyheight)){
		canheight=var.def.ncdf('canopy_height','m',dim=list(xd,yd),
			missval=missing_value,longname='Maximum height of vegetation')
		fluxncvars[[ctr]] = canheight
		ctr = ctr + 1
	}
	# Define site time offset:
	if(!is.na(utcoffset)){
		timeoffset=var.def.ncdf('utc_offset','hours',dim=list(xd,yd),
			missval=missing_value,longname='Local time difference from UTC')
		fluxncvars[[ctr]] = timeoffset
		ctr = ctr + 1
	}
	# Define average precip:
	if(!is.na(avprecip)){
		averageprecip=var.def.ncdf('averagePrecip','mm',dim=list(xd,yd),
			missval=missing_value,longname='Average annual precipitation')
		fluxncvars[[ctr]] = averageprecip
		ctr = ctr + 1
	}
	# Define average temperature:
	if(!is.na(avtemp)){
		averagetemp=var.def.ncdf('averageTemp','K',dim=list(xd,yd),
			missval=missing_value,longname='Average temperature')
		fluxncvars[[ctr]] = averagetemp
		ctr = ctr + 1
	}
	# END VARIABLE DEFINITIONS #########################################
	# Create netcdf file:
	ncid = create.ncdf(fluxfilename,vars=fluxncvars)
	# Write global attributes:
	att.put.ncdf(ncid,varid=0,attname='Production_time',
		attval=as.character(Sys.time()))
	att.put.ncdf(ncid,varid=0,attname='Production_source',
		attval='PALS automated netcdf conversion')
	att.put.ncdf(ncid,varid=0,attname='PALS_fluxtower_template_version',
		attval=templateVersion)
	att.put.ncdf(ncid,varid=0,attname='PALS_dataset_name',
		attval=datasetname)
	att.put.ncdf(ncid,varid=0,attname='PALS_dataset_version',
		attval=datasetversion)
	if(!is.na(vegetationtype)){
		att.put.ncdf(ncid,varid=0,attname='IGBP_vegetation_type',attval=vegetationtype)
	}
	att.put.ncdf(ncid,varid=0,attname='Contact',
		attval='palshelp@gmail.com')
	# Add variable data to file:
	put.var.ncdf(ncid,lat,vals=latitude)
	put.var.ncdf(ncid,lon,vals=longitude)
	# Optional meta data for each site:
	if(!is.na(elevation)) {put.var.ncdf(ncid,elev,vals=elevation)}
	if(!is.na(measurementheight)) {put.var.ncdf(ncid,refheight,vals=measurementheight)}
	if(!is.na(canopyheight)) {put.var.ncdf(ncid,canheight,vals=canopyheight)}
	if(!is.na(utcoffset)) {put.var.ncdf(ncid,timeoffset,vals=utcoffset)}
	if(!is.na(avprecip)) {put.var.ncdf(ncid,averageprecip,vals=avprecip)}
	if(!is.na(avtemp)) {put.var.ncdf(ncid,averagetemp,vals=avtemp)}
	# Time dependent variables:
	if(found$Qle){
		put.var.ncdf(ncid,Qle,vals=datain$data$Qle)
		att.put.ncdf(ncid,Qle,'CF_name','surface_upward_latent_heat_flux')
	}
	if(found$Qh){
		put.var.ncdf(ncid,Qh,vals=datain$data$Qh)
		att.put.ncdf(ncid,Qh,'CF_name','surface_upward_sensible_heat_flux')
	}
	if(found$NEE){
		put.var.ncdf(ncid,NEE,vals=datain$data$NEE)
		att.put.ncdf(ncid,NEE,'CF_name','surface_upward_mole_flux_of_carbon_dioxide **')
		att.put.ncdf(ncid,NEE,'CFname**','Note units are different')
	}
	if(found$Rnet){
		put.var.ncdf(ncid,Rnet,vals=datain$data$Rnet)
		att.put.ncdf(ncid,Rnet,'CF_name','surface_net_downward_radiative_flux')
	}
	if(found$Qg){put.var.ncdf(ncid,Qg,vals=datain$data$Qg)}
	if(found$SWup){
		put.var.ncdf(ncid,SWup,vals=datain$data$SWup)
		att.put.ncdf(ncid,SWup,'CF_name','surface_upwelling_shortwave_flux_in_air')
		att.put.ncdf(ncid,SWnet,'CF_name','surface_net_downward_shortwave_flux')
		att.put.ncdf(ncid,SWnet,'source','defined as SWdown - SWup')
		put.var.ncdf(ncid,SWnet,vals=(datain$data$SWdown - datain$data$SWup))
	}
	if(found$Qle_qc){
		att.put.ncdf(ncid,Qle_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		put.var.ncdf(ncid,Qle_qc,vals=datain$data$QleFlag)
	}
	if(found$Qh_qc){
		att.put.ncdf(ncid,Qh_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		put.var.ncdf(ncid,Qh_qc,vals=datain$data$QhFlag)
	}
	if(found$NEE_qc){
		att.put.ncdf(ncid,NEE_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		put.var.ncdf(ncid,NEE_qc,vals=datain$data$NEEFlag)
	}
	if(found$GPP_qc){
		att.put.ncdf(ncid,GPP_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		put.var.ncdf(ncid,GPP_qc,vals=datain$data$GPPFlag)
	}
	if(found$Rnet_qc){
		att.put.ncdf(ncid,Rnet_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		put.var.ncdf(ncid,Rnet_qc,vals=datain$data$RnetFlag)
	}
	if(found$Qg_qc){
		att.put.ncdf(ncid,Qg_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		put.var.ncdf(ncid,Qg_qc,vals=datain$data$QgFlag)
	}
	if(found$SWup_qc){
		att.put.ncdf(ncid,SWup_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		put.var.ncdf(ncid,SWup_qc,vals=datain$data$SWupFlag)
		att.put.ncdf(ncid,SWnet_qc,'source','defined as min(SWdown_qc - SWup_qc)')
		att.put.ncdf(ncid,SWnet_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		put.var.ncdf(ncid,SWnet_qc,vals=pmin(datain$data$SWdownFlag,datain$data$SWupFlag))
	}
	
	# Close netcdf file:
	close.ncdf(ncid)
}
CreateMetNcFile = function(metfilename,datain,latitude,longitude,timestepsize,
	datasetname,datasetversion,defaultLWsynthesis,found,starttime,
	templateVersion,elevation=NA,measurementheight=NA,canopyheight=NA,
	vegetationtype=NA,utcoffset=NA,avprecip=NA,avtemp=NA){
	# This function, sent observed met variables uploaded by a user
	# in text format, creates a netcdf file which is downloadable for
	# the data provider and is used internally for analysis scripts.
	library(ncdf) # load netcdf library
	missing_value=-9999 # default missing value for all variables
	# Define x, y and z dimensions
	xd = dim.def.ncdf('x',vals=c(1),units='')	
	yd = dim.def.ncdf('y',vals=c(1),units='')
	zd = dim.def.ncdf('z',vals=c(1),units='')
	# Determine data start date and time:
	timeunits = CreateTimeunits(starttime)
	# Create time dimension variable:
	tt=c(0:(datain$ntsteps-1))
	timedata = as.double(tt*timestepsize)
	# Define time dimension:
	td = dim.def.ncdf('time',unlim=TRUE,units=timeunits,vals=timedata)
	# VARIABLE DEFINITIONS ##############################################
	# First, non-time variables:
	# Define latitude:
	lat=var.def.ncdf('latitude','degrees_north',dim=list(xd,yd),
		missval=missing_value,longname='Latitude')
	# Define longitude:
	lon=var.def.ncdf('longitude','degrees_east',dim=list(xd,yd),
		missval=missing_value,longname='Longitude')
	# Define SWdown variable:
	SWdown=var.def.ncdf('SWdown','W/m^2', dim=list(xd,yd,td),
		missval=missing_value,longname='Surface incident shortwave radiation')
	# Define Tair variable:
	Tair=var.def.ncdf('Tair','K', dim=list(xd,yd,zd,td),
		missval=missing_value,longname='Near surface air temperature')
	# Define Rainf variable:
	Rainf=var.def.ncdf('Rainf','mm/s', dim=list(xd,yd,td),
		missval=missing_value,longname='Rainfall rate')
	# Define Qair variable:
	Qair=var.def.ncdf('Qair','kg/kg', dim=list(xd,yd,zd,td),
		missval=missing_value,longname='Near surface specific humidity')
	# Define Wind variable:
	Wind=var.def.ncdf('Wind','m/s', dim=list(xd,yd,zd,td),
		missval=missing_value,longname='Scalar windspeed')
	# Define PSurf variable:
	PSurf=var.def.ncdf('PSurf','Pa', dim=list(xd,yd,td),
		missval=missing_value,longname='Surface air pressure')
	# Define LWdown variable:
	LWdown=var.def.ncdf('LWdown','W/m^2', dim=list(xd,yd,td),
		missval=missing_value,longname='Surface incident longwave radiation')
	metncvars = list(lat,lon,SWdown,Tair,Rainf,Qair,Wind,PSurf,LWdown)
	ctr=10
	# Now optional variables:
	if(found$Snowf){
		# Define Snowf variable:
		Snowf=var.def.ncdf('Snowf','mm/s liq water equivalent', dim=list(xd,yd,td),
			missval=missing_value,longname='Snowfall rate')
		metncvars[[ctr]] = Snowf
		ctr = ctr + 1
	}
	if(found$CO2air){
		# Define CO2air variable:
		CO2air=var.def.ncdf('CO2air','ppmv', dim=list(xd,yd,zd,td),
			missval=missing_value,longname='Near surface CO2 concentration')
		metncvars[[ctr]] = CO2air
		ctr = ctr + 1
	}
	if(found$SWdown_qc){ # Define SWdown_qc variable:
		SWdown_qc=var.def.ncdf('SWdown_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='SWdown quality control flag',prec='integer')
		metncvars[[ctr]] = SWdown_qc
		ctr = ctr + 1
	}
	if(found$Tair_qc){ # Define Tair_qc variable:
		Tair_qc=var.def.ncdf('Tair_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='Tair quality control flag',prec='integer')
		metncvars[[ctr]] = Tair_qc
		ctr = ctr + 1
	}
	if(found$Rainf_qc){ # Define Rainf_qc variable:
		Rainf_qc=var.def.ncdf('Rainf_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='Rainf quality control flag',prec='integer')
		metncvars[[ctr]] = Rainf_qc
		ctr = ctr + 1
	}
	if(found$Qair_qc){ # Define Qair_qc variable:
		Qair_qc=var.def.ncdf('Qair_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='Qair quality control flag',prec='integer')
		metncvars[[ctr]] = Qair_qc
		ctr = ctr + 1
	}
	if(found$Wind_qc){ # Define Wind_qc variable:
		Wind_qc=var.def.ncdf('Wind_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='Wind quality control flag',prec='integer')
		metncvars[[ctr]] = Wind_qc
		ctr = ctr + 1
	}
	if(found$PSurf_qc){ # Define PSurf_qc variable:
		PSurf_qc=var.def.ncdf('PSurf_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='PSurf quality control flag',prec='integer')
		metncvars[[ctr]] = PSurf_qc
		ctr = ctr + 1
	}
	if(found$Snowf_qc){ # Define Snowf_qc variable:
		Snowf_qc=var.def.ncdf('Snowf_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='Snowf quality control flag',prec='integer')
		metncvars[[ctr]] = Snowf_qc
		ctr = ctr + 1
	}
	if(found$CO2air_qc){ # Define CO2air_qc variable:
		CO2air_qc=var.def.ncdf('CO2air_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='CO2air quality control flag',prec='integer')
		metncvars[[ctr]] = CO2air_qc
		ctr = ctr + 1
	}
	if(found$LWdown_qc){ # Define LWdown_qc variable:
		LWdown_qc=var.def.ncdf('LWdown_qc','-', dim=list(xd,yd,td),
			missval=missing_value,longname='LWdown quality control flag',prec='integer')
		metncvars[[ctr]] = LWdown_qc
		ctr = ctr + 1
	}
	# Define elevation:
	if(!is.na(elevation)){
		elev=var.def.ncdf('elevation','m',dim=list(xd,yd),
			missval=missing_value,longname='Site elevation above sea level')
		metncvars[[ctr]] = elev
		ctr = ctr + 1
	}	
	# Define measurement height on tower:
	if(!is.na(measurementheight)){
		refheight=var.def.ncdf('reference_height','m',dim=list(xd,yd),
			missval=missing_value,longname='Measurement height on flux tower')
		metncvars[[ctr]] = refheight
		ctr = ctr + 1
	}
	# Define canopy height:
	if(!is.na(canopyheight)){
		canheight=var.def.ncdf('canopy_height','m',dim=list(xd,yd),
			missval=missing_value,longname='Maximum height of vegetation')
		metncvars[[ctr]] = canheight
		ctr = ctr + 1
	}
	# Define site time offset:
	if(!is.na(utcoffset)){
		timeoffset=var.def.ncdf('utc_offset','hours',dim=list(xd,yd),
			missval=missing_value,longname='Local time difference from UTC')
		metncvars[[ctr]] = timeoffset
		ctr = ctr + 1
	}
	# Define average precip:
	if(!is.na(avprecip)){
		averageprecip=var.def.ncdf('averagePrecip','mm',dim=list(xd,yd),
			missval=missing_value,longname='Average annual precipitation')
		metncvars[[ctr]] = averageprecip
		ctr = ctr + 1
	}
	# Define average temperature:
	if(!is.na(avtemp)){
		averagetemp=var.def.ncdf('averageTemp','K',dim=list(xd,yd),
			missval=missing_value,longname='Average temperature')
		metncvars[[ctr]] = averagetemp
		ctr = ctr + 1
	}

	# END VARIABLE DEFINITIONS #########################################
	# Create netcdf file:
	ncid = create.ncdf(metfilename,vars=metncvars)
	# Write global attributes:
	att.put.ncdf(ncid,varid=0,attname='Production_time',
		attval=as.character(Sys.time()))
	att.put.ncdf(ncid,varid=0,attname='Production_source',
		attval='PALS automated netcdf conversion')
	att.put.ncdf(ncid,varid=0,attname='PALS_fluxtower_template_version',
		attval=templateVersion)
	att.put.ncdf(ncid,varid=0,attname='PALS_dataset_name',
		attval=datasetname)
	att.put.ncdf(ncid,varid=0,attname='PALS_dataset_version',
		attval=datasetversion)
	if(!is.na(vegetationtype)){
		att.put.ncdf(ncid,varid=0,attname='IGBP_vegetation_type',attval=vegetationtype)
	}
	att.put.ncdf(ncid,varid=0,attname='Contact',
		attval='palshelp@gmail.com')
	# Add variable data to file:
	put.var.ncdf(ncid,lat,vals=latitude)
	put.var.ncdf(ncid,lon,vals=longitude)
	# Optional meta data for each site:
	if(!is.na(elevation)) {put.var.ncdf(ncid,elev,vals=elevation)}
	if(!is.na(measurementheight)) {put.var.ncdf(ncid,refheight,vals=measurementheight)}
	if(!is.na(canopyheight)) {put.var.ncdf(ncid,canheight,vals=canopyheight)}
	if(!is.na(utcoffset)) {put.var.ncdf(ncid,timeoffset,vals=utcoffset)}
	if(!is.na(avprecip)) {put.var.ncdf(ncid,averageprecip,vals=avprecip)}
	if(!is.na(avtemp)) {put.var.ncdf(ncid,averagetemp,vals=avtemp)}
	# Time dependent variables:
	put.var.ncdf(ncid,SWdown,vals=datain$data$SWdown)
	att.put.ncdf(ncid,SWdown,attname='CF_name',attval='surface_downwelling_shortwave_flux_in_air')
	put.var.ncdf(ncid,Tair,vals=datain$data$Tair)
	att.put.ncdf(ncid,Tair,attname='CF_name',attval='surface_temperature')
	put.var.ncdf(ncid,Rainf,vals=datain$data$Rainf)
	att.put.ncdf(ncid,Rainf,attname='CF_name',attval='precipitation_flux')
	put.var.ncdf(ncid,Qair,vals=datain$data$Qair)
	att.put.ncdf(ncid,Qair,attname='CF_name',attval='surface_specific_humidity')
	put.var.ncdf(ncid,Wind,vals=datain$data$Wind)
	att.put.ncdf(ncid,Wind,attname='CF_name',attval='wind_speed')
	put.var.ncdf(ncid,PSurf,vals=datain$data$PSurf)
	att.put.ncdf(ncid,PSurf,attname='CF_name',attval='surface_air_pressure')
	if(! found$PSurf){
		att.put.ncdf(ncid,PSurf,'source','Synthesized in PALS based on elevation and temperature')
	}
	put.var.ncdf(ncid,LWdown,vals=datain$data$LWdown)
	att.put.ncdf(ncid,LWdown,attname='CF_name',attval='surface_downwelling_longwave_flux_in_air')
	if(! found$LWdown){
		att.put.ncdf(ncid,LWdown,'source',paste('Entirely synthesized in PALS using',defaultLWsynthesis))
	}else if(found$LWdown & !found$LWdown_all){
		att.put.ncdf(ncid,LWdown,'gapfill_technique',paste('Gap-filled in PALS using',defaultLWsynthesis))
		att.put.ncdf(ncid,LWdown,'gapfill_note','Fluxdata.org template has no QC flag for LWdown - data are assumed original.')
	}
	if(found$Snowf){
		put.var.ncdf(ncid,Snowf,vals=datain$data$Snowf)
	}
	if(found$CO2air){
		put.var.ncdf(ncid,CO2air,vals=datain$data$CO2air)
	}
	if(found$SWdown_qc){
		att.put.ncdf(ncid,SWdown_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		put.var.ncdf(ncid,SWdown_qc,vals=datain$data$SWdownFlag)
	}
	if(found$Tair_qc){
		att.put.ncdf(ncid,Tair_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		put.var.ncdf(ncid,Tair_qc,vals=datain$data$TairFlag)
	}
	if(found$Rainf_qc){
		att.put.ncdf(ncid,Rainf_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		put.var.ncdf(ncid,Rainf_qc,vals=datain$data$RainfFlag)
	}
	if(found$Qair_qc){
		att.put.ncdf(ncid,Qair_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		put.var.ncdf(ncid,Qair_qc,vals=datain$data$QairFlag)
	}
	if(found$Wind_qc){
		att.put.ncdf(ncid,Wind_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		put.var.ncdf(ncid,Wind_qc,vals=datain$data$WindFlag)
	}
	if(found$Snowf_qc){
		att.put.ncdf(ncid,Snowf_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		put.var.ncdf(ncid,Snowf_qc,vals=datain$data$SnowfFlag)
	}
	if(found$PSurf_qc){
		att.put.ncdf(ncid,PSurf_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		put.var.ncdf(ncid,PSurf_qc,vals=datain$data$PSurfFlag)
	}
	if(found$CO2air_qc){
		att.put.ncdf(ncid,CO2air_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		put.var.ncdf(ncid,CO2air_qc,vals=datain$data$CO2airFlag)
	}
	if(found$LWdown_qc){
		att.put.ncdf(ncid,LWdown_qc,'values','As fluxdata.org qcOK: 1 - ok, 0 - ??')
		put.var.ncdf(ncid,LWdown_qc,vals=datain$data$LWdownFlag)
	}
	# Close netcdf file:
	close.ncdf(ncid)
}
ReadTextFluxData = function(fileinname){
	# This function reads tab-delimited text files containing
	# met and flux data from Fluxnet data providers.
	# First get site name, lat, lon, elevation and time step size:
	SiteDetails = scan(file=fileinname,what='list',skip=0,
		nmax=2,quiet=TRUE,sep=',')
	templateVersion = as.character(SiteDetails[2])
	if(!any(KnownTemplateVersions == templateVersion)){
		errtext = paste('S1: Unknown spreadsheet format (please use current PALS version: ',
			CurrentTemplateVersion,').',sep='')
		CheckError(errtext)
	}	
	# Get column names and classes:
	tcol = templateCols(templateVersion)
	# Read flux tower data:
	FluxData = read.csv(file=fileinname,header=FALSE,skip=3,
		colClasses=tcol$classes,col.names=tcol$names)
	# Note number of time steps in data:
	ntsteps = length(FluxData$SWdown)
	if(!(ntsteps>=12 && ntsteps < 1e9)){
		CheckError(paste('S5: Unable to determine number of time steps in:',
			stripFilename(fileinname)))
	}
	# and time step size:
	timestepsize = (FluxData$LocHoD[2]-FluxData$LocHoD[1])*3600
	if( !(timestepsize>=300 && timestepsize<=3600) ){
		CheckError(paste('S5: Unable to ascertain time step size in',
			stripFilename(fileinname)))
	}
	tstepinday=86400/timestepsize # time steps in a day
	ndays = ntsteps/tstepinday # number of days in data set
	# Find starting date / time:
	if(substr(FluxData$LocDate[1],2,2)=='/'){ # i.e. one char day
		sday = as.numeric(substr(FluxData$LocDate[1],1,1))
		if(substr(FluxData$LocDate[1],4,4)=='/'){ # i.e. one char month
			smonth = as.numeric(substr(FluxData$LocDate[1],3,3))
			ystart=5
		}else if(substr(FluxData$LocDate[1],5,5)=='/'){ # i.e. two char month
			smonth = as.numeric(substr(FluxData$LocDate[1],3,4))
			ystart=6
		}else{
			CheckError(paste('S1: Error interpreting data set starting',
				'date from spreadsheet.'))
		}
	}else if(substr(FluxData$LocDate[1],3,3)=='/'){ # i.e. two char day
		sday = as.numeric(substr(FluxData$LocDate[1],1,2))
		if(substr(FluxData$LocDate[1],5,5)=='/'){ # i.e. one char month
			smonth = as.numeric(substr(FluxData$LocDate[1],4,4))
			ystart=6
		}else if(substr(FluxData$LocDate[1],6,6)=='/'){ # i.e. two char month
			smonth = as.numeric(substr(FluxData$LocDate[1],4,5))
			ystart=7
		}else{
			CheckError(paste('S1: Error interpreting data set starting',
				'date from spreadsheet.'))
		}
	}else{
		CheckError(paste('S1: Error interpreting data set starting',
			'date from spreadsheet.'))
	}
	# Create starting year:
	ystr = substr(FluxData$LocDate[1],ystart,nchar(FluxData$LocDate[1]))
	if(nchar(ystr)==2){ # two character yesr
		if(as.numeric(ystr)<60){
			nystr = paste('20',ystr,sep='')
		}else{
			nystr = paste('19',ystr,sep='')
		}
	}else if(nchar(ystr)==4){ # four character year
		nystr = ystr
		# do nothing	
	}else{
		CheckError(paste('S1: Error interpreting data set starting',
			'date from spreadsheet.'))
	}
	syear = as.numeric(nystr)	
	shod = FluxData$LocHoD[1] # starting hour of day
	intyears = Yeardays(syear,ndays)
	# Collate start time variables:
	starttime=list(syear=syear,smonth=smonth,sday=sday,shod=shod)
	# Create list for function exit:
	filedata = list(data=FluxData,ntsteps=ntsteps,
		starttime=starttime,templateVersion=templateVersion,
		timestepsize=timestepsize,ndays=ndays,whole=intyears$whole)
	return(filedata)
}
SynthesizeLWdown=function(TairK,RH,technique){
	if(technique=='Swinbank (1963)'){
		# Synthesise LW down from air temperature only:
		lwdown = 0.0000094*0.0000000567*TairK^6
	}else if(technique=='Brutsaert (1975)'){
		satvapres = 611.2*exp(17.67*((TairK-zeroC)/(TairK-29.65)))
		vapres = pmax(5,RH)/100*satvapres
		emiss = 0.642*(vapres/TairK)^(1/7)
		lwdown = emiss*0.0000000567*TairK^4
	}else if(technique=='Abramowitz (2012)'){
		satvapres = 611.2*exp(17.67*((TairK-zeroC)/(TairK-29.65)))
		vapres = pmax(5,RH)/100*satvapres
		lwdown = 2.648*TairK + 0.0346*vapres - 474
	}else{
		CheckError('S4: Unknown requested LWdown synthesis technique.')
	}
	return(lwdown)
}
gapfillLWdown = function(LWdownIN,TairK,RH,technique){
	# Fills any gaps in LWdown time series using synthesis:
	LWdownOUT = c() # initialise
	LWflag = c() # initialise
	for(t in 1:length(LWdownIN)){
		if(LWdownIN[t]==SprdMissingVal){
			LWdownOUT[t] = SynthesizeLWdown(TairK[t],RH[t],technique)
			LWflag[t] = 0
		}else{
			LWdownOUT[t] = LWdownIN[t]
			LWflag[t] = 1
		}
	}
	return(list(data=LWdownOUT,flag=LWflag))
}

Rel2SpecHum = function(relHum,tk,PSurf){
	# Converts relative humidity to specific humidity.
	# tk - T in Kelvin; PSurf in Pa; relHum as %
	tempC = tk - zeroC
	# Sat vapour pressure in Pa
	esat = 610.78*exp( 17.27*tempC / (tempC + 237.3) )
	# Then specific humidity at saturation:
	ws = 0.622*esat/(PSurf - esat)
	# Then specific humidity:
	specHum = (relHum/100) * ws
	
	return(specHum)
}

Spec2RelHum = function(specHum,tk,PSurf){
	# Converts relative humidity to specific humidity.
	# tk - T in Kelvin; PSurf in Pa; relHum as %
	tempC = tk - zeroC
	# Sat vapour pressure in Pa
	esat = 610.78*exp( 17.27*tempC / (tempC + 237.3) )
	# Then specific humidity at saturation:
	ws = 0.622*esat/(PSurf - esat)
	# Then relative humidity:
	relHum = pmax(pmin(specHum/ws*100, 100),0)
	
	return(relHum)
}
