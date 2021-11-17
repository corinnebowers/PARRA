#!/bin/bash

## set working directory
cd /home/groups/bakerjw/cbowers/PARRA

## create local copies of LISFLOOD input files
cp _data/lisflood/*.asc _scripts/5_INUN/fit_inundation/5c_run_2019event/files

## move to working folder
cd _scripts/5_INUN/fit_inundation/5c_run_2019event
cp ../5b_run_bestfit/files/russian.n.asc files

## load simulation length constant
simlength=$(cat files/simlength.txt)

## create lisflood parfile
bash makepar.sh \
"casestudy"			`#file name` \
"results" 			`#results directory` \
"${simlength}" 			`#simulation length (seconds)` \
"1" 				`#simulation timestep (seconds)` \
"files/russian.dem.asc" 	`#DEM raster file` \
"files/russian.width.asc"	`#channel width raster file` \
"0.03"	 			`#channel roughness coefficient` \
""		 		`#channel depth parameter, r` \
"0.71"		 		`#channel depth parameter, p` \
"files/casestudy.bci"	 	`#.bci file` \
"files/casestudy.bdy" 		`#.bdy file` \
"" 				`#starting .wd file` \
"files/russian.n.asc" 		`#spatially varying floodplain roughness raster` \
"" 				 #constant floodplain roughness coefficient

## add gauge & stage to parfile
echo "gaugefile	files/russian.gauge" >> casestudy.par
echo "stagefile	files/russian.stage" >> casestudy.par

# submit lisflood job with sbatch wrap
sbatch \
--job-name=casestudy \
--output=casestudy.log \
--nodes=1 \
--ntasks=1 \
--cpus-per-task=20 \
--mem=250 \
--time=3:00:00 \
-p cee \
--mail-type=ALL \
--mail-user=cbowers@stanford.edu \
--wrap="ml netcdf; lisflood -v casestudy.par"

