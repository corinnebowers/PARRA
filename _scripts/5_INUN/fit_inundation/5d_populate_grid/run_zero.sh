#!/bin/bash

## load modules
ml R/4 physics
ml proj gdal geos udunits

## execute script(s)
Rscript generate_zero.R

## load simulation length constant
simlength=$(cat files/simlength_zero.txt)

## find the number of samples
i=$(wc -l samples_grid.txt | cut -f1 -d ' ')

## create lisflood parfile
bash makepar.sh \
"grid$i"			`#file name` \
"results" 			`#results directory` \
"${simlength}"			`#simulation length (seconds)` \
"1" 				`#simulation timestep (seconds)` \
"files/russian.dem.asc" 	`#DEM raster file` \
"files/russian.width.asc"	`#channel width raster file` \
"0.030" 			`#channel roughness coefficient` \
""		 		`#channel depth parameter, r` \
"0.71"				`#channel depth parameter, p` \
"files/bci/grid$i.bci" 		`#.bci file` \
""		 		`#.bdy file` \
"" 				`#starting .wd file` \
"files/russian.n.asc"		`#spatially varying floodplain roughness raster` \
"" 				 #constant floodplain roughness coefficient

## modify parfile to speed calculations
sed -i 's/900/21600/g' grid$i.par

## submit lisflood job with sbatch wrap
sbatch \
--job-name=grid$i \
--output=logfiles/grid$i.log \
--nodes=1 \
--ntasks=1 \
--cpus-per-task=20 \
--mem=250 \
--time=2:00:00 \
-p cee,owners \
--mail-type=ALL \
--mail-user=cbowers@stanford.edu \
--wrap="ml netcdf; lisflood -v grid$i.par"
