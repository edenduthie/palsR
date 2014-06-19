library("RJSONIO")
inputFile <- "TestInput_Regional.json"
input <- fromJSON(paste(readLines(inputFile), collapse=""));
Rruntime = system.time(source("RegionalAus0.25Experiment.R"))
print(paste('Time to run:',Rruntime[3]))
output <- toJSON(output)
fileConn<-file("output_Regional.json")
writeLines(output, fileConn)
close(fileConn)