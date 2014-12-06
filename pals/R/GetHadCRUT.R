# GetHadCRUT.R
#
# This script fetches HadCRUT3 monthly surface temperature 
# at5 by 5 degree resolution and filters for missing data.
#
# Gab Abramowitz, UNSW, 2013, gabsun at gmail dot com
#
# Inputs:
# missing_threshold = what percentage of missing data in a grid
# 					cell is acceptable?
#
# Default values are those specified in function arguments:

GetHadCRUT = function(dsetversion,years,missing_threshold){
	cat(paste('Loading HadCRUT version',dsetversion,' \n'))
	library(ncdf) # load packages
	source('~/results/indep_application/functions/missing_data.R')
	remove_missing = TRUE
	if(missing_threshold==100){remove_missing = FALSE}
	meanfile='~/data/global/CRU/absolute.nc' # mean temperatures file
	if(dsetversion==3){
		anfile='~/data/global/CRU/HadCRUT3.nc' # anomaly temperatures file
	}else if(dsetversion==4){
		anfile='~/data/global/CRU/HadCRUT.4.2.0.0.median.nc' # anomaly temperatures file
	}else{
		stop('Unknown version of HadCRUT requested.')	
	}
	# Calculate month count:
	mcount = (years[2]-years[1])*12 + 12
	# Calculate start month
	smonth = (years[1]-1850)*12 + 1
	# Cacluate number of years
	nyears = years[2]-years[1] + 1
	mn=open.ncdf(meanfile,readunlim=FALSE) # open global mean temp file
	an=open.ncdf(anfile,readunlim=FALSE) # open global anomaly temp file
	# Meantemp is of dimension 72 (lon) by 36 (lat) by 12 (month)
	meantemp=get.var.ncdf(mn,'tem')   # read mean temp data
	# Read anomaly temp data for 1970-1999:
	if(dsetversion==3){
		anomtemp=get.var.ncdf(an,'temp',start=c(1,1,1,smonth),count=c(72,36,1,mcount))
	}else if(dsetversion==4){
		anomtemp=get.var.ncdf(an,'temperature_anomaly',start=c(1,1,smonth),count=c(72,36,mcount))
	}
	lat=get.var.ncdf(mn,'lat')   # read latitude values
	lon=get.var.ncdf(mn,'lon')   # read longitude values
	# Close observed data netcdf files:
	close.ncdf(mn)
	close.ncdf(an)
	# Reverse latitude order of mean temp for plotting:
	meantemp = meantemp[,length(meantemp[1,,1]):1,]
	# DO NOT reverse for anomaly variable - opposite latitude order
	# Reverse latitude variable order (from mean temp file):
	lat = -lat
	# Initialise temperature array:
	temp = array(data=NA,dim=c(72,36,mcount))
	# Isolate regions to exclude, if requested:
	if(remove_missing){
		missing=missing_data(years,lon,lat,anomtemp,missing_threshold,paste('HadCRUT',dsetversion,sep=''))
		missingmask=missing$mask
	}else{
		missingmask=matrix(TRUE,length(lon),length(lat))
	}
	gdctr=0 # initialise grid cell counter
	for(i in 1:length(lon)){
		for(j in 1:length(lat)){
			if(missingmask[i,j]){ # where there IS enough data
				gdctr = gdctr + 1 # increment grid cell counter
				# Set NA anomaly values to 0:
				for(m in 1:mcount){
					if(is.na(anomtemp[i,j,m])) {anomtemp[i,j,m]=0}
				}
				# Create observed temperature variable:
				for(k in 1:nyears){	
					temp[i,j,((k-1)*12+1):(k*12)] = meantemp[i,j,] + 
						anomtemp[i,j,((k-1)*12+1):(k*12)]
				}
			} # if missing mask
			
		} # lat 
	} # lon
	obs = list(data=temp,lat=lat,lon=lon,missingmask=missingmask,gdctr=gdctr)
	return(obs)
}