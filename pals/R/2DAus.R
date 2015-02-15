# 2DAus.R
#
# Gab Abramowitz, UNSW, 2015, gabsun at gmail dot com
#
PlotAus = function(lon,lat,data,meanval,sdval,varname,unitstxt,longvarname,zrange,zcols,title,textloc,suppressunits=FALSE){
	# Generates a gridded heatmap style plot for Australia with map, based on input lat/lon.
	library(maps)
	library(mapdata)
	library(fields) # for image.plot
	errtext = 'ok'
	# Decide location of plot for text placement:
	if(textloc=='bottomright'){
		textloc1 = c((lon[1] + (lon[length(lon)] - lon[1])*0.9), (lat[1] + (lat[length(lat)] - lat[1])*0.19) )
		textloc2 = c((lon[1] + (lon[length(lon)] - lon[1])*0.9), (lat[1] + (lat[length(lat)] - lat[1])*0.13) )
		textloc3 = c((lon[1] + (lon[length(lon)] - lon[1])*0.9), (lat[1] + (lat[length(lat)] - lat[1])*0.07) )
	}else if(textloc=='topleft'){
		textloc1 = c((lon[1] + (lon[length(lon)] - lon[1])*0.15), (lat[1] + (lat[length(lat)] - lat[1])*0.94) )
		textloc2 = c((lon[1] + (lon[length(lon)] - lon[1])*0.15), (lat[1] + (lat[length(lat)] - lat[1])*0.88) )
		textloc3 = c((lon[1] + (lon[length(lon)] - lon[1])*0.15), (lat[1] + (lat[length(lat)] - lat[1])*0.82) )
	}
	# Plot:	
	image.plot(lon,lat,data,xlab='Longitude',ylab='Latitude',col=zcols,zlim=zrange,legend.mar=5.5)
	map('worldHires',add=TRUE,wrap=TRUE,xlim=c(min(lon),max(lon)),ylim=c(min(lat),max(lat))) # Add map
	title(title) 
	if(!suppressunits){
		text(x=textloc1[1],y=textloc1[2],labels=unitstxt)
	}
	text(x=textloc2[1],y=textloc2[2],labels=paste('Mean:',signif(meanval,3)))
	text(x=textloc3[1],y=textloc3[2],labels=paste('SD:',signif(sdval,2)))
	return(errtext)
}

DensityLocationAus = function(npanels){
	# Simply prescribes locations for density inset on gridded Australia map plots
	density_location = list()
	if(npanels == 1){
		density_location = list()
		density_location[[1]] = c(0.1,0.5,0.16,0.35)
	}else if(npanels == 2){
		density_location = list()
		density_location[[1]] = c(0.07,0.3,0.17,0.37)
		density_location[[2]] = c(0.57,0.8,0.17,0.37)
	}else if(npanels == 4){
		density_location = list()
		density_location[[1]] = c(0.06,0.3,0.59,0.68)
		density_location[[2]] = c(0.56,0.8,0.59,0.68)
		density_location[[3]] = c(0.06,0.3,0.09,0.18)
		density_location[[4]] = c(0.56,0.8,0.09,0.18)
	}else if(npanels==6){
		density_location = list()
		density_location[[1]] = c(0.04,0.2,0.58,0.67)
		density_location[[2]] = c(0.38,0.54,0.58,0.67)
		density_location[[3]] = c(0.72,0.87,0.58,0.67)
		density_location[[4]] = c(0.04,0.2,0.08,0.17)
		density_location[[5]] = c(0.38,0.54,0.08,0.17)
		density_location[[6]] = c(0.72,0.87,0.08,0.17)
	}
	return(density_location)
}

TextLocationAus = function(npanels){
	if(npanels<5){
		text_location = 'bottomright'
	}else{
		text_location = 'topleft'
	}
}




