#!/bin/bash

## set working directory
cd /home/groups/bakerjw/cbowers/PARRA

## create local copies of LISFLOOD input files
cp _data/lisflood/*.asc _scripts/5_INUN/fit_inundation/5d_populate_grid/files
cd _scripts/5_INUN/fit_inundation/5d_populate_grid
cp ../5b_run_bestfit/files/russian.n.asc files

## create necessary folders
mkdir -p logfiles
mkdir -p results

## load simulation length constant
simlength=$(cat files/simlength.txt)

## find the number of samples
n=$(wc -l samples_grid.txt | cut -f1 -d ' ')
let "n-=1"

for i in $(cat id.txt)
do \
  plusone=$(expr $i + 1)
  line=$(tail -n+$plusone samples_grid.txt | head -n1)

  ## create lisflood parfile
  bash makepar.sh \
  "grid$i"			`#file name` \
  "results" 			`#results directory` \
  "${simlength}"		`#simulation length (seconds)` \
  "1" 				`#simulation timestep (seconds)` \
  "files/russian.dem.asc" 	`#DEM raster file` \
  "files/russian.width.asc"	`#channel width raster file` \
  "0.0300" 			`#channel roughness coefficient` \
  ""		 		`#channel depth parameter, r` \
  ""				`#channel depth parameter, p` \
  "files/bci/grid$i.bci" 	`#.bci file` \
  "files/bdy/grid$i.bdy" 	`#.bdy file` \
  "" 				`#starting .wd file` \
  "files/russian.n.asc"		`#spatially varying floodplain roughness raster` \
  "" 				 #constant floodplain roughness coefficient

  ## calculate SGC files (once)
  if [ $i == 1 ]; then echo "debug" >> gridded$i.par; fi

  ## submit lisflood job with sbatch wrap
  sbatch \
  --job-name=grid${i} \
  --output=logfiles/grid${i}.log \
  --nodes=1 \
  --ntasks=1 \
  --cpus-per-task=20 \
  --mem=250 \
  --time=10:00:00 \
  -p cee,owners \
  --mail-type=ALL \
  --mail-user=cbowers@stanford.edu \
  --wrap="ml netcdf; lisflood -v grid$i.par"

  sleep 2

  ## wait for queue to clear a bit
  jobs=$(squeue -u cbowers -t PENDING | wc -l)
  while [ $jobs -gt 10 ]
  do
    sleep 1m 
    jobs=$(squeue -u cbowers -t PENDING | wc -l)
  done

done
