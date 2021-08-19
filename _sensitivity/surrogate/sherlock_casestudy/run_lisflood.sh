#!/bin/bash

## define calibrated best-fit parameters
SGCn=0.0300
#SGCr=
#SGCp=

## create lisflood parfile
bash makepar.sh \
"casestudy"		`#file name` \
"results" 		`#results directory` \
"3455100" 		`#simulation length (seconds)` \
"1" 			`#simulation timestep (seconds)` \
"russian.dem.asc" 	`#DEM raster file` \
"russian.width.asc"	`#channel width raster file` \
"${SGCn}" 		`#channel roughness coefficient` \
""		 	`#channel depth parameter, r` \
""		 	`#channel depth parameter, p` \
"casestudy.bci"	 	`#.bci file` \
"casestudy.bdy" 	`#.bdy file` \
"" 			`#starting .wd file` \
"russian.n.asc" 	`#spatially varying floodplain roughness raster` \
"" 			 #constant floodplain roughness coefficient

## add gauge & stage to parfile
echo "gaugefile	russian.gauge" >> casestudy.par
echo "stagefile	russian.stage" >> casestudy.par

### note: need to manually edit the parfile so that massint = 900 seconds

# submit lisflood job with sbatch wrap
sbatch \
--job-name=casestudy \
--output=casestudy.log \
--nodes=1 \
--ntasks=1 \
--cpus-per-task=20 \
--mem=250 \
--time=6:00:00 \
-p cee \
--mail-type=ALL \
--mail-user=cbowers@stanford.edu \
--wrap="ml system; ml zlib netcdf; lisflood -v casestudy.par"

