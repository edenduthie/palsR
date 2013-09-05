library("RJSONIO")
inputFile <- "TestInput.json"
input <- fromJSON(paste(readLines(inputFile), collapse=""));
source("SingleSiteExperiment.R")
output <- toJSON(output)
fileConn<-file("output.json")
writeLines(output, fileConn)
close(fileConn)