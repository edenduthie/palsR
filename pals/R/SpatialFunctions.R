# SpatialFunctions.R
#
# Gab Abramowitz, UNSW, 2014, gabsun at gmail dot com
#
# Calculates a vector of grid cell surface areas based 
# on observed data mask.
GridAreaVector = function(obs,mcount){
	source('~/results/indep_application/functions/EarthArea.R')
	area = EarthArea(obs) # get surface area of grid cells
	cat('Creating gridcell surface area vector')
	if(sum(obs$missingmask) == length(obs$missingmask)){ # no missing data in 20C obs
		cat(' without concat')
		surf_areas=c(aperm(array(area$cell,dim=c(length(obs$lon),length(obs$lat),mcount)),c(3,2,1)))
	}else{
		gdctr=0 # initialise grid cell counter
		surf_areas = c()
		#gdctr_ref = matrix(NA,obs$gdctr,2)
		for(i in 1:length(obs$lon)){
			for(j in 1:length(obs$lat)){
				# First check if there is enough obs data for result:
				if(obs$missingmask[i,j]){ # where there IS enough data
					gdctr = gdctr + 1 # increment grid cell counter
					# Save i,j coords of gdctr increment:
					#gdctr_ref[gdctr,1] = i
					#gdctr_ref[gdctr,2] = j
					# vector of surface areas of each grid cell - sam dim as canmat
					surf_areas[((gdctr-1)*mcount+1):(gdctr*mcount)] = area$cell[i,j]
				} # if missing mask
			} # lat
		} # lon
	}
	#return(list(gdctr_ref=gdctr_ref,surf_areas=surf_areas))
	cat('...\n')
	return(list(surf_areas=surf_areas))
}
#
#
# Calculates surface area of Earth and grid cells:
EarthArea = function(obs){
	cellwidth =  obs$lat[2] - obs$lat[1] # assume n degree by n degree
	earthradius=6371
	earth = 4*pi*earthradius^2
	cell = matrix(NA,length(obs$lon),length(obs$lat))
	for(i in 1:length(obs$lon)){
		for(j in 1:length(obs$lat)){
			cell[i,j] = earthradius^2 * cellwidth/360*2*pi * 
				(sin((obs$lat[j]+cellwidth/2)/90*pi/2) - sin((obs$lat[j]-cellwidth/2)/90*pi/2))
		}
	}
	
	if(abs(earth - sum(cell)) > 0.001){
		cat('Earth surface area issue..?!')	
	}
	return(list(cell=cell,earth=earth))
}
#
# Interpolates from one lat-lon global 2D grid to another
# using area weighted averaging
interpolate_areaweight=function(data_in,lat_centre_out,lon_centre_out,global=TRUE){
		
	# record input and output dimension sizes:
	xin=length(data_in$lon)
	yin=length(data_in$lat)
	xout=length(lon_centre_out)
	yout=length(lat_centre_out)
	tin=length(data_in$dat[1,1,])
	
	# If we're already at the exact same grid then return input data:
	grid_equivalence=TRUE # initialise
	for(i in 1:xin){
		if(data_in$lon[i]!=lon_centre_out[i]){
			grid_equivalence	= FALSE
			break # leave for loop
		}
	}
	for(j in 1:yin){
		if(data_in$lat[j]!=lat_centre_out[j]){
			grid_equivalence	= FALSE
			break # leave for loop
		}
	}
	# If they're equivalent, then exit:
	if(grid_equivalence){
		cat('  interpolation: input grid is equivalent to output grid. \n')
		return(data_in$dat)
	}
	
	# check scales are compatible:
	if(abs(data_in$lat[1]-lat_centre_out[1])>20 
		| abs(data_in$lat[yin]-lat_centre_out[yout])>20
		| abs(data_in$lon[1]-lon_centre_out[1])>20 
		| abs(data_in$lon[xin]-lon_centre_out[xout])>20){
	stop('Lat/lon scales seem incompatible between input/output grids')
	}
	
	# initialisations:
	data_out=array(0,c(xout,yout,tin))
	# Define grid boundaries in degrees:
	xwidth_in=360/xin
	ywidth_in=180/yin
	xwidth_out=360/xout
	ywidth_out=180/yout
	
	# Redefine lon_centre_in data_in and xin to account for models that have 
	# centres at 180 and 0 longitude - add an extra two values:
	data_in_mod = array(0,c(xin+2,yin,tin))
	data_in_mod[1,,] = data_in$dat[xin,,] # assign 1st value to be same as 2nd last value
	data_in_mod[2:(xin+1),,] = data_in$dat # middle of lon the same
	data_in_mod[xin+2,,] = data_in$dat[1,,] # assign last value to be same as 2nd value
	
	leftedge_in_tmp=data_in$lon_bnds[1,]
	rightedge_in_tmp=data_in$lon_bnds[2,]
	# Adjust left edge of leftmost cell if needed (may be +170 to +180 deg):
	if(leftedge_in_tmp[1]>120) leftedge_in_tmp[1] = leftedge_in_tmp[1] -360
	
	leftedge_in = c()
	rightedge_in=c()
	# Expand the range of leftedge_in:
	leftedge_in[1] = leftedge_in_tmp[1] - xwidth_in # approximate
	leftedge_in[2:(xin+1)] = leftedge_in_tmp
	leftedge_in[xin+2] = leftedge_in_tmp[xin] + xwidth_in # approximate
	# Expand the range of rightedge_in:
	rightedge_in[1] = rightedge_in_tmp[1] - xwidth_in # approximate
	rightedge_in[2:(xin+1)] = rightedge_in_tmp
	rightedge_in[xin+2] = rightedge_in_tmp[xin] + xwidth_in # approximate
	xin_mod = xin + 2
	topedge_in=data_in$lat_bnds[2,]
	botedge_in=data_in$lat_bnds[1,]
	leftedge_out=lon_centre_out-xwidth_out/2  # left HS of grid cells
	rightedge_out=lon_centre_out+xwidth_out/2 # left HS of grid cells
	topedge_out=lat_centre_out+ywidth_out/2   # top of grid cells
	botedge_out=lat_centre_out-ywidth_out/2      # bottom of grid cells
	
	# Find input grid cells that have edges within output grid cells:
	for(i in 1:xout){
		for(j in 1:yout){ # for each output grid cell
			frac_total=0 # initialise
			written_x=FALSE
			# Find bottom left search area in input grid:
			xstart=max(1,as.integer(((i-2)/xout)*xin_mod))
			ystart=max(1,as.integer(((j-2)/yout)*yin))
			# count through input grid cells in this area
			for(ii in xstart:xin_mod){
				xoverlap=0 # initialise
				# Check if there's any overlap in x dimension:
				# 1. check if right edge is in output cell boundaries:
				if((rightedge_in[ii]>leftedge_out[i]) & (rightedge_in[ii]<=rightedge_out[i])){
					xoverlap=min(rightedge_in[ii]-leftedge_in[ii],rightedge_in[ii]-leftedge_out[i])
				# 2. check if left edge is in output cell boundaries:
				}else if((leftedge_in[ii]>leftedge_out[i]) & (leftedge_in[ii]<rightedge_out[i])){
					xoverlap=min(rightedge_in[ii]-leftedge_in[ii],rightedge_out[i]-leftedge_in[ii])
				# 3. check if input cell entirely envelopes output cell in x dim
				}else if((leftedge_in[ii]<=leftedge_out[i]) & (rightedge_in[ii]>=rightedge_out[i])){
					xoverlap=rightedge_out[i]-leftedge_out[i]
				}else if(written_x){ # i.e. no longer any x overlap
					break # leave ii in xstart:xin for loop; current output grid cell is done
				}
				written_y=FALSE
				for(jj in ystart:yin){ 
					yoverlap=0 # initialise
					# 1. check if top edge is in output cell boundaries:
					if(topedge_in[jj]>botedge_out[j] & topedge_in[jj]<=topedge_out[j]){
						yoverlap=min(topedge_in[jj]-botedge_in[jj],topedge_in[jj]-botedge_out[j])
					# 2. check if bottom edge is in output cell boundaries:
					}else if(botedge_in[jj]>botedge_out[j] & botedge_in[jj]<topedge_out[j]){
						yoverlap=min(topedge_in[jj]-botedge_in[jj],topedge_out[j]-botedge_in[jj])
					# 3. check if input cell entirely envelopes output cell in y dim 
					}else if(botedge_in[jj]<=botedge_out[j] & topedge_in[jj]>=topedge_out[j]){
						yoverlap=topedge_out[j]-botedge_out[j]
					}else if(written_y){ # i.e. no longer any y overlap
						break # leave jj in ystart:yin for loop
					}
					# If there is overlap, add it to total:
					if(xoverlap>0 & yoverlap >0){
						# calculate fraction of output grid cell covered by this input cell:
						frac = (xoverlap*yoverlap)/(xwidth_out*ywidth_out)
						frac_total = frac_total + frac
						data_out[i,j,] = data_out[i,j,] + data_in_mod[ii,jj,]*frac
						written_x=TRUE
						written_y=TRUE
					}		
				} # ystart:yin
			} # xstart:xin
			if(abs(frac_total-1)>0.0001){
				cat('***** Frac total /= 1.0 ***** for (x,y)=',i,j,'\n')
				stop(frac_total)
			}
		} # 1:yout
	} # 1:xout
		
	return(data_out)

}