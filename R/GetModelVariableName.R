# GetModelVariableName.R
#
# This function finds the name of a requested variable in a 
# LSM output file.
# Gab Abramowitz UNSW 2010 (palshelp at gmail dot com)
#
GetModelVariableName = function(mfid,varname,units){
	varexists = FALSE # initialise
	unitsexist = FALSE # initialise
	nvars = length(mfid$var) # number of variables in netcdf file
	# Check to see if variable exists:
	# [The following approach is not efficient, but error handling in
	# the ncdf package was hard to work with...]
	for (n in 1:length(varname)){ # search each possible variable name
		for (v in 1:nvars){ # Search through all variables in netcdf file
			if(mfid$var[[v]]$name==varname[n]){ # if variable found
				varexists = TRUE
				# Set model variable name to be the same as that requested:
				name = varname[n]
				# Then find and check variable units:
				mvunits=att.get.ncdf(mfid,name,'units')
				for (u in 1:length(units$name)){
					if(mvunits$value==units$name[u]){
						unitsexist = TRUE # note units have been found
						# Set units adjustments appropriately:
						multiplier = units$multiplier[u]
						addition = units$addition[u]
						units=mvunits$value
						break # out of units name for loop
					}
				}
				if(!unitsexist){ # i.e. didn't recognise variable units
					multiplier=NA
					addition=NA
					units=mvunits$value
					CheckError(paste('M3: Did not recognise units for',
						mfid$var[[v]]$name,'in model output file'))
				}	
				break # i.e. we've already found the variable
			} # if variable found
		}
		if(varexists){break} # i.e. we've already found the variable
	}
	if(!varexists){# i.e. didn't find variable
		name=varname[1]
		multiplier=NA
		addition=NA
		units=NA
		CheckError(paste('M3: Could not find variable equivalent to',varname[1],
			'in model output file'))
	}	
	# Variables returned:
	modelvar = list(name=name,multiplier=multiplier,units=units,
		addition=addition,exists=varexists,unitsexist=unitsexist)
	return(modelvar)
}