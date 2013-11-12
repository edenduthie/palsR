library(pals)

print(input["_id"])
files <- input[["files"]]

for (i in 1:(length(files)-1)  ) {
    file <- files[[i]]
    if( file[['type']] == "ModelOutput" ) {
        modelOutputFilename = file[['filename']];
    }
    else if( file[['type']] == "DataSet") {
        if( file[['component']] == "flux" ) {
            fluxFilename = file[['filename']];
        }
    }
}

print(paste("Model Output: ",modelOutputFilename));
print(paste("Flux: ",fluxFilename));

variables = list('NEE','Qg','Qh','Qle','Rnet','SWnet');
unitList = list(NEEUnits,QgUnits,QhUnits,RnetUnits,SWnetUnits);
unitNameList = list(NEEUnitsName,QgUnitsName,QhUnitsName,RnetUnitsName,SWnetUnitsName);

output = list(files=list());

for( i in 1:(length(variables)-1) ) {

    varname <- variables[[i]];
    units <- unitList[[i]];
    unitsName <- unitNameList[[i]][[1]];
    ytext=paste("Average ",varname," flux ",unitsName);
    xtext=paste(varname,unitsName);
    legendtext=c('Observed','Modelled')
    obs = GetFluxnetVariable(varname,fluxFilename,units)
    model = GetModelOutput(varname,modelOutputFilename,units)
    CheckTiming(model$timing,obs$timing)
    acdata=matrix(NA,length(model$data),2)
    acdata[,1] = obs$data
    acdata[,2] = model$data
    analysisType = 'ModelAnalysis';
    
    outfile <- setOutput(analysisType);
    print(paste("Outfile ",outfile));
    AnnualCycle(varname,acdata,varname,ytext,legendtext, obs$timing$tstepsize,obs$timing$whole,'no');
    fileType = paste(varname,"AnnualCycle");
    file = list(type=fileType,filename=paste(getwd(),outfile,sep = "/"),mimetype="image/png");
    output[["files"]][[length(output[["files"]])+1]] <- file
    
    #outfile <- setOutput(analysisType);
    #print(paste("Outfile ",outfile));
    #AveragingWindow(analysisType,analysisType,model$data,obs$data,varname,ytext,obs$timing$tstepsize);
    #fileType = paste(varname,"AvWindow");
    #file = list(type=fileType,filename=paste(getwd(),outfile,sep = "/"),mimetype="image/png");
    #output[["files"]][[length(output[["files"]])+1]] <- file
    
    outfile <- setOutput(analysisType);
    print(paste("Outfile ",outfile));
    dcdata=matrix(NA,length(obs$data),1)
	dcdata[,1] = obs$data
    if(obs$qcexists){
		vqcdata = matrix(NA,length(obs$data),1)
		vqcdata[,1] = obs$qc
		# Call diurnal cycle plotting function:
		DiurnalCycle(analysisType,dcdata,varname,ytext,legendtext,
			obs$timing$tstepsize,obs$timing$whole,analysisType,
			vqcdata=vqcdata)
	}else{
		# Call diurnal cycle plotting function:
		DiurnalCycle(analysisType,dcdata,varname,ytext,legendtext,
			obs$timing$tstepsize,obs$timing$whole,analysisType)
	}
    fileType = paste(varname,"DiurnalCycle");
    file = list(type=fileType,filename=paste(getwd(),outfile,sep = "/"),mimetype="image/png");
    output[["files"]][[length(output[["files"]])+1]] <- file
    
    outfile <- setOutput(analysisType);
    print(paste("Outfile ",outfile));
    # Create data matrix for function:
	pdfdata=matrix(NA,length(model$data),2)
	pdfdata[,1] = obs$data
	pdfdata[,2] = model$data
	nbins=500
	# Check if obs QC/gap-filling data exists, and if so, send to plotting function:
	if(obs$qcexists){
		vqcdata = matrix(NA,length(obs$data),1)
		vqcdata[,1] = obs$qc
		# Call Timeseries plotting function:
		PALSPdf(analysisType,pdfdata,varname,ytext,
			legendtext,obs$timing,nbins,analysisType,vqcdata=vqcdata)
	}else{
		# Call Timeseries plotting function:
		PALSPdf(analysisType,pdfdata,varname,ytext,
			legendtext,obs$timing,nbins,analysisType)
	}
    fileType = paste(varname,"Pdf");
    file = list(type=fileType,filename=paste(getwd(),outfile,sep = "/"),mimetype="image/png");
    output[["files"]][[length(output[["files"]])+1]] <- file
    
    outfile <- setOutput(analysisType);
    print(paste("Outfile ",outfile));
    if(obs$qcexists){
		vqcdata = matrix(NA,length(obs$data),1)
		vqcdata[,1] = obs$qc
		# Call plotting function with qc data:
		PALSScatter(analysisType,model$data,obs$data,varname,varname,
			legendtext,obs$timing$tstepsize,obs$timing$whole,
			modlabel=analysisType,vqcdata=vqcdata)
	}else{
		# Call plotting function without qc data:
		PALSScatter(analysisType,model$data,obs$data,varname,varname,
			legendtext,obs$timing$tstepsize,obs$timing$whole,modlabel=analysisType)
	}
    fileType = paste(varname,"Scatter");
    file = list(type=fileType,filename=paste(getwd(),outfile,sep = "/"),mimetype="image/png");
    output[["files"]][[length(output[["files"]])+1]] <- file
    
    outfile <- setOutput(analysisType);
    print(paste("Outfile ",outfile));
    TaylorDiagram(analysisType,model$data,obs$data,
		varname,xtext,obs$timing$tstepsize,obs$timing$whole,analysisType)
    fileType = paste(varname,"Taylor");
    file = list(type=fileType,filename=paste(getwd(),outfile,sep = "/"),mimetype="image/png");
    output[["files"]][[length(output[["files"]])+1]] <- file
    
    outfile <- setOutput(analysisType);
    print(paste("Outfile ",outfile));
    #Create data matrix for function:
	tsdata=matrix(NA,length(obs$data),1)
	tsdata[,1] = obs$data
	plotcex = 1.0 # plot text magnification factor
	winsize = 14
	# Check if obs QC/gap-filling data exists, and if so, send to plotting function:
	if(obs$qcexists){
		vqcdata = matrix(NA,length(obs$data),1)
		vqcdata[,1] = obs$qc
		# Call Timeseries plotting function with qc data:
		Timeseries(analysisType,tsdata,varname,ytext,
			legendtext,plotcex,obs$timing,smoothed=TRUE,winsize,
			vqcdata=vqcdata)
	}else{
		# Call Timeseries plotting function without qc data:
		Timeseries(analysisType,tsdata,varname,ytext,
			legendtext,plotcex,obs$timing,smoothed=TRUE,winsize)
	}
    fileType = paste(varname,"Taylor");
    file = list(type=fileType,filename=paste(getwd(),outfile,sep = "/"),mimetype="image/png");
    output[["files"]][[length(output[["files"]])+1]] <- file
}

outfile <- setOutput(analysisType);
print(paste("Outfile ",outfile));
units=QleUnits
legendtext = c('Rnet - Qg','Qle + Qh')
# Load modelled latent heat data:
varname=QleNames
mod_qle = GetModelOutput(varname,modelOutputFilename,units)
# Load modelled sensible heat data:
varname=QhNames
mod_qh = GetModelOutput(varname,modelOutputFilename,units)
# Load modelled net radiation data:
varname=RnetNames
mod_rnet = GetModelOutput(varname,modelOutputFilename,units)
# Load modelled ground heat flux data:
varname=QgNames
mod_qg = GetModelOutput(varname,modelOutputFilename,units)
# Create data vectors for scatter function:
qleqh = mod_qle$data + mod_qh$data
rnetqg = mod_rnet$data - mod_qg$data
# Call PALSScatter plotting function:
PALSScatter(analysisType,qleqh,rnetqg,
	'Energy density','energy conservation',legendtext,
	mod_qg$timing$tstepsize,mod_qg$timing$whole,ebal=TRUE,
	analysisType)
file = list(type=fileType,filename=paste(getwd(),outfile,sep = "/"),mimetype="image/png");
output[["files"]][[length(output[["files"]])+1]] <- file

print(output[["files"]][[1]]$type)