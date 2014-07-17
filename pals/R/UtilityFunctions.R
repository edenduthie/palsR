# UtilityFuntions.R
#
# Utility functions for PALS R package
#
# Gab Abramowitz UNSW 2014 (palshelp at gmail dot com)
#

ChooseColours = function(range,variablename,plottype,diffthreshold=NULL){
	# Returns a colour range for gridded plots
	library(colorRamps)
	
	# Full / most range:
	red2blue = colorRampPalette(c('red','orange','yellow','green','blue'))
	yellow2purpleCool = colorRampPalette(c('yellow','green3','blue','darkorchid4'))
	yellow2purpleWarm = colorRampPalette(c('yellow','red','magenta'))
	purple2yellowWarm = colorRampPalette(c('magenta','red','yellow'))
	iceblue2green = colorRampPalette(c('slategray1','midnightblue','blue','green3','green'))
	green2iceblue = colorRampPalette(c('green','green3','blue','midnightblue','slategray1'))
	
	# Half range:
	green2darkblue = colorRampPalette(c('green','green4','blue','midnightblue'))
	darkblue2green = colorRampPalette(c('midnightblue','blue','green4','green'))
	darkred2yellow = colorRampPalette(c('red4','red','orange','yellow'))
	yellow2darkred = colorRampPalette(c('yellow','orange','red','red4'))
	
	# Small range:
	yellow2red = colorRampPalette(c('yellow','red'))
	red2yellow = colorRampPalette(c('red','yellow'))
	green2blue = colorRampPalette(c('green','blue'))
	blue2green = colorRampPalette(c('blue','green'))
	
	coolvars = c('Qle','Evap')
	warmvars = c('Tair','Qh','Rnet','SWdown','SWnet')
	colourres = 36 # approximately how many colours in a plot (will control size of white space if diff plot)
	
	# If no difference threshold has been specified, use 5%:
	if(is.null(diffthreshold)){
		diffthreshold = (range[2] - range[1]) / 20
	}
	
	# Assess cases where colours for a difference plot are requested first:
	if(plottype=='difference'){
		# i.e. the plot will contain a zero that we want coloured white
		# First check that we really do need a difference plot:
		if(range[1] > (-1*diffthreshold)){
			# Just use a positive scale
			plottype = 'positive'
		}else if(range[2]<diffthreshold){
			# Just use a negative scale
			plottype = 'negative'
		}
		# Find fraction of range below 0
		lowfrac = abs(range[1]) / (abs(range[1]) + range[2])
		lownum = floor(lowfrac * colourres)
		# Find fraction of range above 0
		highfrac = range[2] / (abs(range[1]) + range[2])
		highnum = floor(highfrac * colourres)
		# Decide colour range:
		if(any(warmvars == variablename)){ # For variables warm colours when positive
			if(lowfrac/highfrac > 2){ # most fo the range is below 0
				colours = c(iceblue2green(lownum),'#FFFFFF','#FFFFFF',yellow2red(highnum))
			}else if(lowfrac/highfrac < 1/2){ # most of the range is above 0
				colours = c(blue2green(lownum),'#FFFFFF','#FFFFFF',yellow2purpleWarm(highnum))
			}else{
				colours = c(darkblue2green(lownum),'#FFFFFF','#FFFFFF',yellow2darkred(highnum))
			}
		}else if(any(coolvars == variablename)){ # For variables cool colours when positive
			if(lowfrac/highfrac > 2){ # most fo the range is below 0
				colours = c(purple2yellowWarm(lownum),'#FFFFFF','#FFFFFF',green2blue(highnum))
			}else if(lowfrac/highfrac < 1/2){ # most of the range is above 0
				colours = c(red2yellow(lownum),'#FFFFFF','#FFFFFF',green2iceblue(highnum))
			}else{
				colours = c(darkred2yellow(lownum),'#FFFFFF','#FFFFFF',green2darkblue(highnum))
			}
		}
	}
	
	# Now assess cases where just a positive or negative scale is required:
	if((plottype=='positive') && (any(coolvars == variablename))){
		colours = yellow2purpleCool(colourres)
	}else if((plottype=='positive') && (any(warmvars == variablename))){
		colours = yellow2purpleWarm(colourres)
	}else if((plottype=='negative') && (any(coolvars == variablename))){
		colours = purple2yellowWarm(colourres)
	}else if((plottype=='negative') && (any(warmvars == variablename))){
		colours = iceblue2green(colourres)
	}
	
	return(colours)
}

# Function for crashing semi-gracefully:
CheckError = function(errtext,errcode='U1:'){
	if(errtext != 'ok'){
		# Additionally report command line call
		calltext = paste(commandArgs(),collapse=' ')
		alltext = paste(errtext,calltext)
		# If error, write to std error
		cat(alltext,' ^ \n',file=stderr()); stop(alltext,call. = FALSE)
	}
}

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

NumberOfBenchmarks = function(bench,Bctr){
	# Determines the number of user nominated benchmarks in a call to an 
	# experiment script, as well as the number of files associated with each.
	# Bctr - total number of benchmark files
	# bench - contains data for each file
	if(Bctr == 0){
		nBench = 0
		benchfiles = NA
	}else{
		nBench = 1
		# Determine number of user nominated benchmarks:
		for(b in 1:Bctr){
			nBench = max(nBench, as.integer(bench[[Bctr]]$number))
		}
		# Store which files belong to which benchmark:
		benchfiles = list()
		bexists = c(0)
		for(b in 1:Bctr){
			benchnumber = as.integer(bench[[b]]$number)
			if(any(bexists == benchnumber)){
				benchfiles[[benchnumber]] = c(benchfiles[[benchnumber]] , b)
			}else{
				benchfiles[[benchnumber]] = c(b)
				bexists = c(bexists,benchnumber)
			}
		}
	}
	result = list(number = nBench, benchfiles=benchfiles)
	return(result)
}
#
# Strips path from filename: 
stripFilename = function(fpath) {
	fsplit = strsplit(fpath,'/')
	fcharvec = as.character(fsplit[[1]])
	fname = fcharvec[length(fcharvec)]
	return(fname)
}
#
# Set raster output graphics file resolution:
getResolution = function(analysisType){
	if(analysisType=='default'){
    	iwidth=1100
    	iheight=800
    }else if(analysisType=='ObsAnalysis'){
    	iwidth=900
    	iheight=600
    }else if(analysisType=='QCplotsSpreadsheet'){
    	iwidth=900
    	iheight=600
    }else if(analysisType=='BenchAnalysis'){
    	iwidth=900
    	iheight=600
    }else{
    	CheckError('I2: Unknown analysis type requested in getResolution.')
    }
    ires = list(width=iwidth,height=iheight)
    return(ires)
}
#
# Set output file type:
setOutput = function(analysisType) {
	outtype = 'png'
	outfilename = paste(uuid(), outtype, sep='.')
	ires = getResolution('default')
#	if(analysisType=='QCplotsSpreadsheet'){
#		fsize = 24	
#	}else{
		fsize = ceiling(ires$width / 1500 * 24) # set font size
#	}
	# Set output file type, if not to screen:
	if (outtype == 'pdf' ) {
		pdf(file=outfilename, paper='a4r', width=11, height=8)
	}else if (outtype=='ps') {
		postscript(file=outfilename, paper='special', width=11, height=8)
	}else if (outtype == 'png') {
		png(file=outfilename, width=ires$width, height=ires$height, pointsize=fsize)
	}else if(outtype == 'jpg'){
		jpeg(file=outfilename, width=ires$width, height=ires$height, pointsize=fsize)
	}else{
		CheckError(paste('I1: Requested output format not recognised:',outtype))
	}
	return(outfilename);
}

# UUID generator:
uuid = function(uppercase=FALSE) {
	## Version 4 UUIDs have the form xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx
	## where x is any hexadecimal digit and y is one of 8, 9, A, or B
	## e.g., f47ac10b-58cc-4372-a567-0e02b2c3d479
 
	hex_digits <- c(as.character(0:9), letters[1:6])
	hex_digits <- if (uppercase) toupper(hex_digits) else hex_digits
	 
	y_digits <- hex_digits[9:12]
	 
	paste(
	  paste0(sample(hex_digits, 8, replace=TRUE), collapse=''),
	  paste0(sample(hex_digits, 4, replace=TRUE), collapse=''),
	  paste0('4', paste0(sample(hex_digits, 3, replace=TRUE), collapse=''), collapse=''),
	  paste0(sample(y_digits,1), paste0(sample(hex_digits, 3, replace=TRUE), collapse=''), collapse=''),
	  paste0(sample(hex_digits, 12, replace=TRUE), collapse=''),
	  sep='-')
}
