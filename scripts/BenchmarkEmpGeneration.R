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


removeflagged = TRUE # only use non-gapfilled data?

varnames=c(QleNames[1],QhNames[1],NEENames[1],QgNames[1])
varsunits=c(QleUnits$name[1],QhUnits$name[1],
	NEEUnits$name[1],QgUnits$name[1])

PLUMBER_met = c(
	'~/Documents/admin/PALS/datasets/ObsNc/AmpleroFluxnet.1.4_met.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/BlodgettFluxnet.1.4_met.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/BugacFluxnet.1.4_met.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/ElSalerFluxnet.1.4_met.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/ElSaler2Fluxnet.1.4_met.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/EspirraFluxnet.1.4_met.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/FortPeckFluxnet.1.4_met.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/HarvardFluxnet.1.4_met.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/HesseFluxnet.1.4_met.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/HowardFluxnet.1.4_met.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/HowlandmFluxnet.1.4_met.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/HyytialaFluxnet.1.4_met.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/KrugerFluxnet.1.4_met.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/LoobosFluxnet.1.4_met.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/MerbleueFluxnet.1.4_met.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/MopaneFluxnet.1.4_met.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/PalangFluxnet.1.4_met.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/SylvaniaFluxnet.1.4_met.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/TumbaFluxnet.1.4_met.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/UniMichFluxnet.1.4_met.nc'
	)
PLUMBER_flux = c(
	'~/Documents/admin/PALS/datasets/ObsNc/AmpleroFluxnet.1.4_flux.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/BlodgettFluxnet.1.4_flux.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/BugacFluxnet.1.4_flux.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/ElSalerFluxnet.1.4_flux.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/ElSaler2Fluxnet.1.4_flux.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/EspirraFluxnet.1.4_flux.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/FortPeckFluxnet.1.4_flux.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/HarvardFluxnet.1.4_flux.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/HesseFluxnet.1.4_flux.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/HowardFluxnet.1.4_flux.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/HowlandmFluxnet.1.4_flux.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/HyytialaFluxnet.1.4_flux.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/KrugerFluxnet.1.4_flux.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/LoobosFluxnet.1.4_flux.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/MerbleueFluxnet.1.4_flux.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/MopaneFluxnet.1.4_flux.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/PalangFluxnet.1.4_flux.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/SylvaniaFluxnet.1.4_flux.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/TumbaFluxnet.1.4_flux.nc',
	'~/Documents/admin/PALS/datasets/ObsNc/UniMichFluxnet.1.4_flux.nc'
	)

TrainPathsMet = c()
TrainPathsFlux = c()
PredictPathMet = '~/Documents/admin/PALS/datasets/ObsNc/TumbaFluxnet.1.3_met.nc'
DataSetName = 'TumbaFluxnet'
DataSetVersion = '1.3'
PredictNcPath = '~/Documents/admin/PALS/testing/BenchTumbaFluxnet.nc'
SiteLat = -35.656
SiteLon = 148.15 

GenerateBenchmarkSet(TrainPathsMet,TrainPathsFlux,PredictPathMet,
	DatSetName,DataSetVersion,PredictNcPath,SiteLat,SiteLon,
	varnames,varsunits,removeflagged)