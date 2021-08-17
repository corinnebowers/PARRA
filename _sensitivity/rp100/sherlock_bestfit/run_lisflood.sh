#!/bin/bash

# read parameters for the ith iteration
SGCn=0.024
#SGCr=$(echo $line | cut -f19 -d ' ')
#SGCp=$(echo $line | cut -f18 -d ' ')

# create lisflood parfile
bash makepar.sh \
"bestfit"		`#file name` \
"results" 		`#results directory` \
"6048000" 		`#simulation length (seconds)` \
"1" 			`#simulation timestep (seconds)` \
"russian.dem.asc" 	`#DEM raster file` \
"russian.width.asc"	`#channel width raster file` \
"${SGCn}" 		`#channel roughness coefficient` \
""			`#channel depth parameter, r` \
"" 			`#channel depth parameter, p` \
"bestfit.bci"		`#.bci file` \
"bestfit.bdy"	 	`#.bdy file` \
"" 			`#starting .wd file` \
"russian.n.asc" 	`#spatially varying floodplain roughness raster` \
"" 			 #constant floodplain roughness coefficient

# submit lisflood job with sbatch wrap
sbatch \
--job-name=bestfit \
--output=bestfit.log \
--nodes=1 \
--ntasks=1 \
--cpus-per-task=20 \
--mem=250 \
--time=4:00:00 \
-p cee \
--mail-type=ALL \
--mail-user=cbowers@stanford.edu \
--wrap="ml netcdf; lisflood -v bestfit.par"

