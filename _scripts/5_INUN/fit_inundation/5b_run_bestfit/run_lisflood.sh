#!/bin/bash

## set working directory
cd /home/groups/bakerjw/cbowers/PARRA

## create local copies of LISFLOOD input files
cp _data/lisflood/*.asc _scripts/5_INUN/fit_inundation/5b_run_bestfit/files

## create necessary folders
cd _scripts/5_INUN/fit_inundation/5b_run_bestfit
mkdir -p results

## load simulation length constant
simlength=$(cat files/simlength.txt)

# create lisflood parfile
bash makepar.sh \
"bestfit"			`#file name` \
"results" 			`#results directory` \
"${simlength}" 			`#simulation length (seconds)` \
"1" 				`#simulation timestep (seconds)` \
"files/russian.dem.asc" 	`#DEM raster file` \
"files/russian.width.asc"	`#channel width raster file` \
"0.018" 			`#channel roughness coefficient` \
"0.430"		 		`#channel depth parameter, r` \
""				`#channel depth parameter, p` \
"files/bestfit.bci"		`#.bci file` \
"files/bestfit.bdy"	 	`#.bdy file` \
"" 				`#starting .wd file` \
"files/russian.n.asc"	 	`#spatially varying floodplain roughness raster` \
"" 				 #constant floodplain roughness coefficient

# submit lisflood job with sbatch wrap
sbatch \
--job-name=bestfit \
--output=run_lisflood.log \
--nodes=1 \
--ntasks=1 \
--cpus-per-task=20 \
--mem=250 \
--time=4:00:00 \
-p cee \
--mail-type=ALL \
--mail-user=cbowers@stanford.edu \
--wrap="ml netcdf; lisflood -v bestfit.par"

