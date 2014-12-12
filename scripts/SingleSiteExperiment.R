# Master script for single site flux tower based experiments
# Gab Abramowitz, UNSW, 2014 (palshelp at gmail dot com)

library(pals)
library(parallel)

print(input["_id"])
files <- input[["files"]]

# Retrieve model output, forcing and evaluation data set and benchmark location and 
# meta data from javascript input list: 
ModelOutputFiles = list()
ForcingDataSetFiles = list()
EvalDataSetFiles = list()
BenchmarkFiles = list()
MOctr = 0
FDSctr = 0
EDSctr = 0
Bctr = 0
for (i in 1:(length(files))  ) {
    file <- files[[i]]
    if( file[['type']] == "ModelOutput" ) {
    	MOctr = MOctr + 1
        ModelOutputFiles[[MOctr]] = list(path=file[['path']],mimetype=file[['mimetype']],
        	name=file[['name']])
    }else if( (file[['type']] == "DataSet")) {
    	EDSctr = EDSctr + 1
        EvalDataSetFiles[[EDSctr]] = list(path=file[['path']],mimetype=file[['mimetype']],
        	name=file[['name']])
    }else if( file[['type']] == "Benchmark") {
    	Bctr = Bctr + 1
        BenchmarkFiles[[Bctr]] = list(path=file[['path']],mimetype=file[['mimetype']],
        	name=file[['name']],number=file[['number']]) # user rank of benchmark
    }
}

#print(paste("Model Output file: ",ModelOutputFiles[[1]][['path']]));
#print(paste("Data Set file: ",EvalDataSetFiles[[1]][['path']]));

# Nominate variables to analyse here (use ALMA standard names) - fetches
# alternate names, units, units transformations etc:
vars = GetVariableDetails(c('NEE','Qle','Qh','Rnet','Qg','SWnet'))

# Analyses that can apply to any variable:
analyses = c('Taylor','Timeseries','AnnualCycle','DiurnalCycle','Scatter','PDF')

# Determine number of user-nominated benchmarks:
BenchInfo = BenchmarkInfo(BenchmarkFiles,Bctr)

# Set up analysis data and analysis list so we can use lapply or parlapply:
AnalysisData = list()
AnalysisList = list()

# Load all variables from obs and model output
analysis_number = 0
for(v in 1:length(vars)){
	obs = GetFluxnetVariable(vars[[v]],EvalDataSetFiles[[1]])
    model = GetModelOutput(vars[[v]],ModelOutputFiles)
    bench = GetBenchmarks(vars[[v]],BenchmarkFiles,BenchInfo)
	# Save model, obs, bench data for each variable:
	AnalysisData[[v]] = list(obs=obs, model=model, bench = bench)
	# Add those analyses that are equally applicable to any variable to analysis list:
	for(a in 1:length(analyses)){
		analysis_number = (v-1)*length(analyses) + a
		AnalysisList[[analysis_number]] = list(vindex=v, type=analyses[a])
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

# Draw summary metric table here:
tableOut = MetricTableSingleSite(outinfo,BenchInfo)

# Write outinfo to output list for javascript:
output = list(files=outinfo);

#check error propagation!

for(i in 1: length(output[["files"]])){
	cat('Output ',i,': \n')
	cat('  type:',output[["files"]][[i]]$type)
	cat('  filename:',output[["files"]][[i]]$filename,'\n')
	cat('  bench error:',output[["files"]][[i]]$bencherror,'\n')
	cat('  first metric for model - ',output[["files"]][[i]]$metrics[[1]]$name,':',
		output[["files"]][[i]]$metrics[[1]]$model_value,'\n')
}