 # Utility functions for PALS R package
# Gab Abramowitz UNSW 2014 (palshelp at gmail dot com)
#

# Sending error to std err:
CheckError = function(errtext,errcode='U1:'){
	if(errtext != 'ok'){
		# Additionally report command line call
		calltext = paste(commandArgs(),collapse=' ')
		alltext = paste(errtext,calltext)
		# If error, write to std error
		cat(alltext,' ^ \n',file=stderr()); stop(alltext,call. = FALSE)
	}
}

NumberOfBenchmarks = function(bench,Bctr){
	# Determines the number of user nominated benchmarks in a call to an 
	# experiment script, as well as the number of files associated with each:
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
				bexists = c(bexists,b)
			}
		}
	}
	result = list(number = nBench, benchfiles=benchfiles)
	return(result)
}

getOutType = function(analysisType) {
    if(analysisType=='ModelAnalysis'){
    	outtype='png'
    }else if(analysisType=='ObsAnalysis'){
    	outtype=commandArgs()[7]
    }else if(analysisType=='QCplotsSpreadsheet'){
    	outtype=commandArgs()[7]
    }else if(analysisType=='BenchAnalysis'){
    	outtype=commandArgs()[10]
    }else{
    	CheckError('I2: Unknown analysis type requested in getOutType.')
    }
    return(outtype)
}

###
# Generate the outfile name from label and type
###
getOutFileName = function(outtype,analysisType) {
    paste(uuid(), outtype, sep='.')
}

#####
# Get resolution of rasterised output file (if any)
###
getResolution = function(analysisType){
	if(analysisType=='ModelAnalysis'){
    	iwidth=900
    	iheight=600
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

###
# Sets the output, with optional 'size' argument
# in case of rasterized files.
###
setOutput = function(analysisType) {
	outtype = getOutType(analysisType)
	outfilename = getOutFileName(outtype,analysisType)
	ires = getResolution(analysisType)
	if(analysisType=='QCplotsSpreadsheet'){
		fsize = 24	
	}else{
		fsize = ceiling(ires$width / 1500 * 24) # set font size
	}
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
# Copies current graphics device, if pdf, to png device.
copyOutput = function(analysisType) {
	outtype = getOutType(analysisType)
	if(outtype == 'pdf'){
		#library(Cairo)
		# Create png outfilename:
		outfilename = getOutFileName('png',analysisType)
		ires = getResolution(analysisType) # find resolution
		#fsize = ceiling(ires$width / 1500 * 24) # set font size
		#Cairo(width=ires$width, height=ires$height,file=outfilename,
		#	type='png',pointsize=fsize)
		#png(file=outfilename,width=ires$width, height=ires$height,pointsize=fsize,type="quartz")
		#dev.set(dev.prev()) # back to pdf device
		#dev.copy()
		dev2bitmap(file=outfilename,units='px',width=ires$width, height=ires$height,
			pointsize=fsize)
	} # otherwise do nothing
}

##########
# Strips path from filename: 
stripFilename = function(fpath) {
	fsplit = strsplit(fpath,'/')
	fcharvec = as.character(fsplit[[1]])
	fname = fcharvec[length(fcharvec)]
	return(fname)
}


uuid <- function(uppercase=FALSE) {
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
