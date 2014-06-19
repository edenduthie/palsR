# Master script for Australian continental 0.25 x 0.25 degree experiment
# Gab Abramowitz, UNSW, 2014 (palshelp at gmail dot com)

library(pals)
library(parallel)

print(input["_id"])
files <- input[["files"]]

# Retrieve model output, forcing and evaluation data set and benchmark location and 
# meta data from javascript input list: 
ModelOutputs = list()
ForcingDataSets = list()
EvalDataSets = list()
Benchmarks = list()
MOctr = 0
FDSctr = 0
EDSctr = 0
Bctr = 0
for (i in 1:(length(files))  ) {
    file <- files[[i]]
    if( file[['type']] == "ModelOutput" ) {
    	MOctr = MOctr + 1
        ModelOutputs[[MOctr]] = list(path=file[['path']],mimetype=file[['mimetype']],
        	name=file[['name']])
    }else if( (file[['type']] == "DataSet")) {
    	EDSctr = EDSctr + 1
        EvalDataSets[[EDSctr]] = list(path=file[['path']],mimetype=file[['mimetype']],
        	name=file[['name']])
    }else if( file[['type']] == "Benchmark") {
    	Bctr = Bctr + 1
        Benchmarks[[Bctr]] = list(path=file[['path']],mimetype=file[['mimetype']],
        	name=file[['name']],number=file[['number']]) # user rank of benchmark
    }
}

print(paste("Model Output file: ",ModelOutputs[[1]][['path']]));
print(paste("Data Set file: ",EvalDataSets[[1]][['path']]));

# Nominate variables to analyse here (use ALMA standard names) - fetches
# alternate names, units, units transformations etc:
vars = GetVariableDetails(c('Qle'))

# Analyses that can apply to any variable:
genAnalysis = c('Mean') #,'TempCorr','PDFall','PDF2D','RMSE','Taylor')

# Determine number of user-nominated benchmarks:
nBench = NumberOfBenchmarks(Benchmarks,Bctr)

# Set up analysis data and analysis list so we can use lapply or parlapply:
AnalysisList = list()
OutInfo = list()

# Create cluster:
cl = makeCluster(getOption('cl.cores', detectCores()))
#cl = makeCluster(getOption('cl.cores', 4))

# Load all variables from obs and model output
for(v in 1:length(vars)){
	obs = GetFluxnetVariable(vars[[v]],EvalDataSets[[1]])
    model = GetModelOutput(vars[[v]],ModelOutputs)
    bench = GetBenchmarks(vars[[v]],Benchmarks,nBench)
	# Add those analyses that are equally applicable to any variable to analysis list:
	for(a in 1:length(genAnalysis)){
		AnalysisList[[a]] = list(vindex=v, type=genAnalysis[a])
	}
	OutInfo[[v]] = parLapply(cl=cl,AnalysisList,DistributeGriddedAnalyses,vars=vars,
		obs=obs,model=model,bench=bench)
}
# Add multiple variable analysis to analysis list:
# analysis_number = analysis_number + 1
# AnalysisList[[analysis_number]] = list(vindex=0, type='EvapFrac')
# analysis_number = analysis_number + 1
# AnalysisList[[analysis_number]] = list(vindex=0, type='Conserve')

# stop cluster
stopCluster(cl)

# Write outinfo to output list for javascript:
output = list(files=outinfo);

#check error propagation!

for(i in 1: length(output[["files"]])){
	print(output[["files"]][[i]]$type)
	print(output[["files"]][[i]]$filename)
	print(output[["files"]][[i]]$error)
	print(output[["files"]][[i]]$bencherror)
}