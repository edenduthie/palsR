# GetGPCP.R
#
# This script fetches GPCP monthly precipitation 
# at 2.5 by 2.5 degree resolution and filters for missing data.
#
# Gab Abramowitz, UNSW, 2013, gabsun at gmail dot com
#

GetGPCP = function(dsetversion,years,missing_threshold){
	cat(paste('Loading GPCP version',dsetversion,' - no missing data. \n'))
	library(ncdf) # load packages
	if(dsetversion==2.2){
		precipfile='~/data/global/GPCP/precip.mon.mean2.2.nc' 
	}else{
		stop('Unknown version of GPCP requested.')	
	}
	# Calculate month count:
	mcount = (years[2]-years[1])*12 + 12
	# Calculate start month
	smonth = (years[1]-1979)*12 + 1
	# Cacluate number of years
	nyears = years[2]-years[1] + 1
	pf=open.ncdf(precipfile,readunlim=FALSE) # open global mean temp file
	# Meantemp is of dimension 72 (lon) by 36 (lat) by 12 (month)
	precip=get.var.ncdf(pf,'precip',start=c(1,1,smonth),count=c(144,72,mcount))   # read mean temp data
	lat_orig=get.var.ncdf(pf,'lat')   # read latitude values
	lon_orig=get.var.ncdf(pf,'lon')   # read longitude values
	close.ncdf(pf)
	
	# transform lat and lon (and data to match)
	lat = -lat_orig
	lon = c()
	lon[1:72] = lon_orig[73:144] - 360
	lon[73:144] = lon_orig[1:72]
	p1=array(dim=dim(precip))
	prec=array(dim=dim(precip))
	p1[,1:72,] = precip[,72:1,]
	prec[1:72,,] = p1[73:144,,]
	prec[73:144,,] = p1[1:72,,]
	
	# simply for consistency - no missing data
	missingmask=matrix(TRUE,length(lon),length(lat))
	gdctr = 144*72 # grid cell counter - just for consistency since no missing data here
	obs = list(data=prec,lat=lat,lon=lon,missingmask=missingmask,gdctr=gdctr)
	return(obs)
}