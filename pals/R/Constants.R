# Constants.R
#
# Gab Abramowitz, UNSW, 2014 (palshelp at gmail dot com)
#
zeroC = 273.15
SprdMissingVal=-9999 # missing value in spreadsheet
NcMissingVal=-9999 # missing value in created netcdf files
CurrentTemplateVersion = '1.0.2'
KnownTemplateVersions = c('1.0.1','1.0.2')
#
# Variable order in spreadsheet template versions:
templateCols = function(templateVersion = CurrentTemplateVersion){
	if(templateVersion=='1.0.1'){
		columnnames=c('LocDate','LocHoD','SWdown',
		'SWdownFlag','LWdown','LWdownFlag','Tair','TairFlag','Qair',
		'QairFlag','Wind','WindFlag','Rainf','RainfFlag','Snowf',
		'SnowfFlag','PSurf','PSurfFlag','CO2air','CO2airFlag','Rnet',
		'RnetFlag','SWup','SWupFlag','Qle','QleFlag','Qh',
		'QhFlag','NEE','NEEFlag','Qg','QgFlag')
		cclasses = c('character','numeric','numeric',
		'integer','numeric','integer','numeric','integer','numeric',
		'integer','numeric','integer','numeric','integer','numeric',
		'integer','numeric','integer','numeric','integer','numeric',
		'integer','numeric','integer','numeric','integer','numeric',
		'integer','numeric','integer','numeric','integer')
	}else if(templateVersion=='1.0.2'){
		columnnames=c('LocDate','LocHoD','SWdown',
		'SWdownFlag','LWdown','LWdownFlag','Tair','TairFlag','Qair',
		'QairFlag','Wind','WindFlag','Rainf','RainfFlag','Snowf',
		'SnowfFlag','PSurf','PSurfFlag','CO2air','CO2airFlag','Rnet',
		'RnetFlag','SWup','SWupFlag','Qle','QleFlag','Qh',
		'QhFlag','NEE','NEEFlag','GPP','GPPFlag','Qg','QgFlag')
		cclasses = c('character','numeric','numeric',
		'integer','numeric','integer','numeric','integer','numeric',
		'integer','numeric','integer','numeric','integer','numeric',
		'integer','numeric','integer','numeric','integer','numeric',
		'integer','numeric','integer','numeric','integer','numeric',
		'integer','numeric','integer','numeric','integer','numeric','integer')
	}else{
		CheckError('S3: Unknown template version [function: templateCols]')
	}
	tcols = list(names=columnnames,classes=cclasses)
	return(tcols)
}
# Gets index of a template variable 
varIndex = function(varname,templateVersion){
	tcols = templateCols(templateVersion)
	idx = 0
	for(v in 1:length(tcols$names)){
		if(varname == tcols$names[v]){
			idx = v	
		}
	}
	return(idx)	
}

