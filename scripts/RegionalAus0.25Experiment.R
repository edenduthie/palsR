# Master script for Australian continental 0.25 x 0.25 degree experiment
# Gab Abramowitz, UNSW, 2014 (palshelp at gmail dot com)

library(pals)
library(parallel)

print(paste('ID:',input["_id"]))
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

# Nominate variables to analyse here (use ALMA standard names) - fetches
# alternate names, units, units transformations etc:
vars = GetVariableDetails(c('Qle'))

# Analyses that can apply to any variable:
genAnalysis = c('TimeMean','TimeSD','TimeRMSE','TimeCor') #'PDFall','PDF2D','Taylor')

# Determine number of user-nominated benchmarks:
nBench = BenchmarkInfo(BenchmarkFiles,Bctr)

# Set up analysis data and analysis list so we can use lapply or parlapply:
AnalysisList = list()

# Load all variables from obs and model output
for(v in 1:length(vars)){
	obs = GetGLEAM_Aus(vars[[v]],EvalDataSetFiles,force_interval='monthly')
    model = GetModelOutput(vars[[v]],ModelOutputFiles)  
    bench = GetBenchmarks(vars[[v]],BenchmarkFiles,nBench)
    
	# Add those analyses that are equally applicable to any variable to analysis list:
	for(a in 1:length(genAnalysis)){
		analysis_number = (v-1)*length(genAnalysis) + a
		AnalysisList[[analysis_number]] = list(vindex=v, type=genAnalysis[a])
	}
}

# Create cluster:
#cl = makeCluster(getOption('cl.cores', detectCores()))
#cl = makeCluster(getOption('cl.cores', 2))

	OutInfo = lapply(AnalysisList,DistributeGriddedAnalyses,vars=vars,
		obs=obs,model=model,bench=bench)
#	OutInfo = parLapply(cl=cl,AnalysisList,DistributeGriddedAnalyses,vars=vars,
#		obs=obs,model=model,bench=bench)

# stop cluster
#stopCluster(cl)

# Write outinfo to output list for javascript:
output = list(files=OutInfo);

for(i in 1: length(output[["files"]])){
	cat('Output ',i,': \n')
	cat('  type:',output[["files"]][[i]]$type,'\n')
	cat('  filename:',output[["files"]][[i]]$filename,'\n')
	cat('  bench error:',output[["files"]][[i]]$bencherror,'\n')
	cat('  first metric for model - ',output[["files"]][[i]]$metrics[[1]]$name,':',
		output[["files"]][[i]]$metrics[[1]]$model_value,'\n')
}