# BenchmarkEmpGeneration.R
#
# Runs empirical benchmark simulations
#
# Gab Abramowitz CCRC, UNSW 2014 (palshelp at gmail dot com)
#
library(pals)

### change saved benchmark format so that it's more like model output (and can be uploaded as such)
### parallelise benchmark generation
### use each LSM's Rnet to constrain emp fluxes with bowen ratio?

setwd('~/Documents/admin/PALS/scripts/palsR/scripts/')
removeflagged = TRUE # only use non-gapfilled data?

varnames=c(QleNames[1],QhNames[1],NEENames[1],QgNames[1])
varsunits=c(QleUnits$name[1],QhUnits$name[1],
	NEEUnits$name[1],QgUnits$name[1])

PLUMBER_met = c(
	'~/data/flux_tower/PALS/ObsNc/AmpleroFluxnet.1.4_met.nc',
	'~/data/flux_tower/PALS/ObsNc/BlodgettFluxnet.1.4_met.nc',
	'~/data/flux_tower/PALS/ObsNc/BugacFluxnet.1.4_met.nc',
	'~/data/flux_tower/PALS/ObsNc/ElSalerFluxnet.1.4_met.nc',
	'~/data/flux_tower/PALS/ObsNc/ElSaler2Fluxnet.1.4_met.nc',
	'~/data/flux_tower/PALS/ObsNc/EspirraFluxnet.1.4_met.nc',
	'~/data/flux_tower/PALS/ObsNc/FortPeckFluxnet.1.4_met.nc',
	'~/data/flux_tower/PALS/ObsNc/HarvardFluxnet.1.4_met.nc',
	'~/data/flux_tower/PALS/ObsNc/HesseFluxnet.1.4_met.nc',
	'~/data/flux_tower/PALS/ObsNc/HowardFluxnet.1.4_met.nc',
	'~/data/flux_tower/PALS/ObsNc/HowlandmFluxnet.1.4_met.nc',
	'~/data/flux_tower/PALS/ObsNc/HyytialaFluxnet.1.4_met.nc',
	'~/data/flux_tower/PALS/ObsNc/KrugerFluxnet.1.4_met.nc',
	'~/data/flux_tower/PALS/ObsNc/LoobosFluxnet.1.4_met.nc',
	'~/data/flux_tower/PALS/ObsNc/MerbleueFluxnet.1.4_met.nc',
	'~/data/flux_tower/PALS/ObsNc/MopaneFluxnet.1.4_met.nc',
	'~/data/flux_tower/PALS/ObsNc/PalangFluxnet.1.4_met.nc',
	'~/data/flux_tower/PALS/ObsNc/SylvaniaFluxnet.1.4_met.nc',
	'~/data/flux_tower/PALS/ObsNc/TumbaFluxnet.1.4_met.nc',
	'~/data/flux_tower/PALS/ObsNc/UniMichFluxnet.1.4_met.nc'
	)
PLUMBER_flux = c(
	'~/data/flux_tower/PALS/ObsNc/AmpleroFluxnet.1.4_flux.nc',
	'~/data/flux_tower/PALS/ObsNc/BlodgettFluxnet.1.4_flux.nc',
	'~/data/flux_tower/PALS/ObsNc/BugacFluxnet.1.4_flux.nc',
	'~/data/flux_tower/PALS/ObsNc/ElSalerFluxnet.1.4_flux.nc',
	'~/data/flux_tower/PALS/ObsNc/ElSaler2Fluxnet.1.4_flux.nc',
	'~/data/flux_tower/PALS/ObsNc/EspirraFluxnet.1.4_flux.nc',
	'~/data/flux_tower/PALS/ObsNc/FortPeckFluxnet.1.4_flux.nc',
	'~/data/flux_tower/PALS/ObsNc/HarvardFluxnet.1.4_flux.nc',
	'~/data/flux_tower/PALS/ObsNc/HesseFluxnet.1.4_flux.nc',
	'~/data/flux_tower/PALS/ObsNc/HowardFluxnet.1.4_flux.nc',
	'~/data/flux_tower/PALS/ObsNc/HowlandmFluxnet.1.4_flux.nc',
	'~/data/flux_tower/PALS/ObsNc/HyytialaFluxnet.1.4_flux.nc',
	'~/data/flux_tower/PALS/ObsNc/KrugerFluxnet.1.4_flux.nc',
	'~/data/flux_tower/PALS/ObsNc/LoobosFluxnet.1.4_flux.nc',
	'~/data/flux_tower/PALS/ObsNc/MerbleueFluxnet.1.4_flux.nc',
	'~/data/flux_tower/PALS/ObsNc/MopaneFluxnet.1.4_flux.nc',
	'~/data/flux_tower/PALS/ObsNc/PalangFluxnet.1.4_flux.nc',
	'~/data/flux_tower/PALS/ObsNc/SylvaniaFluxnet.1.4_flux.nc',
	'~/data/flux_tower/PALS/ObsNc/TumbaFluxnet.1.4_flux.nc',
	'~/data/flux_tower/PALS/ObsNc/UniMichFluxnet.1.4_flux.nc'
	)

TrainPathsMet = PLUMBER_met[1:10]
TrainPathsFlux = PLUMBER_flux[1:10]
PredictPathMet = PLUMBER_met[19]
PredictNcPath = 'BenchTumbaTest_1lin.nc'

benchx = list(c('SWdown'),c('SWdown','Tair'),c('SWdown','Tair','Qair'))
benchtype = c('mlr','mlr','kmeans')
benchparam = c(0,0,27)

binfo=list(x=c('SWdown','Tair'),type='mlr',par=0)

if(any(TrainPathsMet == PredictPathMet)){
	outofsample = FALSE
}else{
	outofsample = TRUE	
}

GenerateEmpBenchmark(TrainPathsMet,TrainPathsFlux,PredictPathMet,
	PredictNcPath,varnames,varsunits,removeflagged,binfo,outofsample)