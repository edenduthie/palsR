# Colours.R
#
# Functions deciding PALS plot colours
#
# Gab Abramowitz UNSW 2014 (palshelp at gmail dot com)
#
LineColours = function() {
	# For line plots:
	plotcolours=c('black','blue2','indianred3','gold2','yellowgreen')
}
LineColours2 = function() {
	# For line plots:
	plotcolours2=DesaturateColours(LineColours(),sat=0.5)
}

TableWinColour = function() {
	'lightgreen'	
}
TableModelColour = function() {
	'gray87'	
}
TableBenchmarkColour = function() {
	'gray93'	
}
BenchmarkColours = function(bench,plotobs=TRUE){
	# Returns vector of colours for lines in a plot.
	# If no obs line in the plot (e.g. error plot), first index will be model, else obs
	# If for some reason a benchmark failed (e.g. missing variable), colours are adjusted to make them 
	# consistent across different plots 
	plotcolours = c()
	if(plotobs){ # i.e. obs line will be part of the plot
		plotcolours[1] = LineColours()[1]
	}
	plotcolours = c(plotcolours, LineColours()[2])
	if(bench$exist){
		plotcolours = c(plotcolours, LineColours()[ (bench$index[1] + 2) ])
		if(bench$howmany == 2){
			plotcolours = c(plotcolours, LineColours()[ (bench$index[2] + 2) ])
		}else if(bench$howmany == 3){
			plotcolours = c(plotcolours, LineColours()[ (bench$index[2] + 2) ])
			plotcolours = c(plotcolours, LineColours()[ (bench$index[3] + 2) ])
		}
	}
	return(plotcolours)
}
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

#library(RColorBrewer) ## For some example colors

# Function for desaturating colors by specified proportion
DesaturateColours = function(colours, sat) {
	library(colorspace) 
    desat_hsv = diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(colours))
    result = hsv(desat_hsv[1,], desat_hsv[2,], desat_hsv[3,])
    return(result)
}
