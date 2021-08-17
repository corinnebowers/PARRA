#!/bin/bash

# find the number of samples
n=$(wc -l samples_rp100_500.txt | cut -f1 -d ' ')
let "n-=1"

for i in $(seq 1 $n)
do \
plusone=$(expr $i + 1)
line=$(tail -n+$plusone samples_rp100_500.txt | head -n1)

# read parameters for the ith iteration
SGCn=$(echo $line | cut -f16 -d ' ')
SGCr=$(echo $line | cut -f19 -d ' ')
SGCp=$(echo $line | cut -f18 -d ' ')

# move increment upwards
j=$i
let "j+=250"

# create lisflood parfile
bash makepar.sh \
"rpflow$j"			`#file name` \
"results" 			`#results directory` \
"6048000" 			`#simulation length (seconds)` \
"1" 				`#simulation timestep (seconds)` \
"russian.dem.asc" 		`#DEM raster file` \
"russian.width.asc"		`#channel width raster file` \
"${SGCn}" 			`#channel roughness coefficient` \
"${SGCr}"	 		`#channel depth parameter, r` \
"${SGCp}"			`#channel depth parameter, p` \
"bci_bdy/rpflow$j.bci" 		`#.bci file` \
"bci_bdy/rpflow$j.bdy"	 	`#.bdy file` \
"" 				`#starting .wd file` \
"manning/russian.n$j.asc" 	`#spatially varying floodplain roughness raster` \
"" 				 #constant floodplain roughness coefficient

# submit lisflood job with sbatch wrap
sbatch \
--job-name=lfrp${j} \
--output=logfiles/lisflood_${j}.log \
--nodes=1 \
--ntasks=1 \
--cpus-per-task=20 \
--mem=250 \
--time=4:00:00 \
-p owners,cee \
--mail-type=ALL \
--mail-user=cbowers@stanford.edu \
--wrap="ml netcdf; lisflood -v rpflow$j.par"

sleep 2

# wait for queue to clear a bit
jobs=$(squeue -u cbowers -t PENDING | wc -l)
while [ $jobs -gt 10 ]
do
  sleep 1m 
  jobs=$(squeue -u cbowers -t PENDING | wc -l)
done

done
