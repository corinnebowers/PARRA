#!/bin/bash

## set working directory
cd /home/groups/bakerjw/cbowers/PARRA

## create local copies of LISFLOOD input files
cp _data/lisflood/*.asc _scripts/5_INUN/fit_inundation/5a_fit_lisflood/files

## create necessary folders
cd _scripts/5_INUN/fit_inundation/5a_fit_lisflood
mkdir -p logfiles
mkdir -p results

## load simulation length constant
simlength=$(cat files/simlength.txt)

## find the number of samples
n=$(wc -l samples_lisflood.txt | cut -f1 -d ' ')
let "n-=1"

## submit lisflood jobs within for loop
for i in $(seq 753 $n)
do \
  plusone=$(expr $i + 1)
  line=$(tail -n+$plusone samples_lisflood.txt | head -n1)

  ## read parameters for the ith iteration
  SGCn=$(echo $line | cut -f16 -d ' ')
  SGCr=$(echo $line | cut -f19 -d ' ')
  SGCp=$(echo $line | cut -f18 -d ' ')

  ## create lisflood .par file
  bash makepar.sh \
  "fitrp$i"			`#file name` \
  "results" 			`#results directory` \
  "${simlength}"		`#simulation length (seconds)` \
  "1" 				`#simulation timestep (seconds)` \
  "files/russian.dem.asc" 	`#DEM raster file` \
  "files/russian.width.asc"	`#channel width raster file` \
  "${SGCn}"			`#channel roughness coefficient` \
  "${SGCr}"			`#channel depth parameter, r` \
  "${SGCp}"			`#channel depth parameter, p` \
  "files/bci/lisflood$i.bci"	`#.bci file` \
  "files/bdy/lisflood$i.bdy"	`#.bdy file` \
  "" 				`#starting .wd file` \
  "files/lulc/russian.n$i.asc"	`#spatially varying floodplain roughness raster` \
  "" 				 #constant floodplain roughness coefficient

  ## calculate SGC files (once)
  if [ $i == 1 ]; then echo "debug" >> fit_lisflood_$i.par; fi

  ## submit lisflood job with sbatch wrap
  sbatch \
  --job-name=lfrp${i} \
  --output=logfiles/fitrp$i.log \
  --nodes=1 \
  --ntasks=1 \
  --cpus-per-task=20 \
  --mem=250 \
  --time=4:00:00 \
  -p owners,cee \
  --mail-type=ALL \
  --mail-user=cbowers@stanford.edu \
  --wrap="ml netcdf; lisflood -v fitrp$i.par"

  sleep 2

  ## wait for queue to clear a bit
  jobs=$(squeue -u cbowers -t PENDING | wc -l)
  while [ $jobs -gt 50 ]
  do
    sleep 1m 
    jobs=$(squeue -u cbowers -t PENDING | wc -l)
  done

done
