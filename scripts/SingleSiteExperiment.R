# Master script for single site flux tower based experiments
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
vars = GetVariableDetails(c('Qle','Qh','NEE','Rnet','Qg','SWnet'))

# Analyses that can apply to any variable:
genAnalysis = c('AvWindow','Scatter','Timeseries','AnnualCycle','DiurnalCycle','PDF','Taylor')

# Determine number of user-nominated benchmarks:
nBench = NumberOfBenchmarks(Benchmarks,Bctr)

# Set up analysis data and analysis list so we can use lapply or parlapply:
AnalysisData = list()
AnalysisList = list()

# Load all variables from obs and model output
analysis_number = 0
for(v in 1:length(vars)){
	obs = GetFluxnetVariable(vars[[v]],EvalDataSets[[1]])
    model = GetModelOutput(vars[[v]],ModelOutputs)
    bench = GetBenchmarks(vars[[v]],Benchmarks,nBench)
	# Save model, obs, bench data for each variable:
	AnalysisData[[v]] = list(obs=obs, model=model, bench = bench)
	# Add those analyses that are equally applicable to any variable to analysis list:
	for(a in 1:length(genAnalysis)){
		analysis_number = (v-1)*length(genAnalysis) + a
		AnalysisList[[analysis_number]] = list(vindex=v, type=genAnalysis[a])
	}
}
# Add multiple variable analysis to analysis list:
# analysis_number = analysis_number + 1
# AnalysisList[[analysis_number]] = list(vindex=0, type='EvapFrac')
# analysis_number = analysis_number + 1
# AnalysisList[[analysis_number]] = list(vindex=0, type='Conserve')

# Create cluster:
cl = makeCluster(getOption('cl.cores', detectCores()))
#cl = makeCluster(getOption('cl.cores', 4))

# Process analyses using lapply:
#outinfo = lapply(AnalysisList,DistributeSingleSiteAnalyses,data=AnalysisData,vars=vars)
outinfo = parLapply(cl=cl,AnalysisList,DistributeSingleSiteAnalyses,data=AnalysisData,vars=vars)

# stop cluster
stopCluster(cl)

# Write outinfo to output list for javascript:
output = list(files=outinfo);

#check error propagation!

for(i in 1: length(output[["files"]])){
	cat('Output ',i,': \n')
	cat('  type:',output[["files"]][[i]]$type,'\n')
	cat('  filename:',output[["files"]][[i]]$filename,'\n')
	cat('  bench error:',output[["files"]][[i]]$bencherror,'\n')
#	cat('  first metric for model - ',output[["files"]][[i]]$metrics[[1]]$name,':',
#		output[["files"]][[i]]$metrics[[1]]$model_value,'\n')
}