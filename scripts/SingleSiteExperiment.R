library(pals)

print(input["_id"])
files <- input[["files"]]

varname=NEENames
units=NEEUnits
ytext=expression("Average NEE flux "~mu~"mol/"~m^{2}~"/s")
legendtext=c('Observed','Modelled')


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

obs = GetFluxnetVariable(varname,fluxFilename,units)
model = GetModelOutput(varname,modelOutputFilename,units)
CheckTiming(model$timing,obs$timing)
#acdata=matrix(NA,length(model$data),2)
#acdata[,1] = obs$data
#acdata[,2] = model$data

#AnnualCycle(getObsLabel(analysisType),acdata,varname,ytext,legendtext,
#    obs$timing$tstepsize,obs$timing$whole,getModLabel(analysisType))


png(filename="averageWindow.png")
cars <- c(1, 3, 6, 4, 9)
plot(cars)
dev.off()
output = list(files=list(list(type="NEEAnnualCycle",filename="averageWindow.png",mimetype="image/png")))
print(output[["files"]][[1]]$type)