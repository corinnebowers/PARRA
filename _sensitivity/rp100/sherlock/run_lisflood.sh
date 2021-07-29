#!/bin/bash

# find the number of samples
n=$(wc -l samples_rp100.txt | cut -f1 -d ' ')
let "n-=1"

for i in $(seq 1 $n)
do \
plusone=$(expr $i + 1)
line=$(tail -n+$plusone samples_rp100.txt | head -n1)

# read parameters for the ith iteration
SGCn=$(echo $line | cut -f16 -d ' ')
SGCr=$(echo $line | cut -f19 -d ' ')

# create lisflood parfile
bash makepar.sh \
"rpflow$i"		`#file name` \
"results" 		`#results directory` \
"6048000" 		`#simulation length (seconds)` \
"1" 			`#simulation timestep (seconds)` \
"russian.dem.asc" 	`#DEM raster file` \
"russian.width.asc"	`#channel width raster file` \
"${SGCn}" 		`#channel roughness coefficient` \
"${SGCr}"	 	`#channel depth parameter` \
"bci_bdy/rpflow$i.bci" 	`#.bci file` \
"bci_bdy/rpflow$i.bdy" 	`#.bdy file` \
"" 			`#starting .wd file` \
"manning/russian.n$i.asc" 	`#spatially varying floodplain roughness raster` \
"" 			 #constant floodplain roughness coefficient

# submit lisflood job with sbatch wrap
sbatch \
--job-name=lfrp${i} \
--output=logfiles/lisflood_${i}.log \
--nodes=1 \
--ntasks=1 \
--cpus-per-task=20 \
--mem=250 \
--time=4:00:00 \
-p owners \
--mail-type=ALL \
--mail-user=cbowers@stanford.edu \
--wrap="ml netcdf; lisflood -v rpflow$i.par"

sleep 2

# wait for queue to clear a bit
jobs=$(squeue -u cbowers -t PENDING | wc -l)
while [ $jobs -gt 10 ]
do
  sleep 10m 
  jobs=$(squeue -u cbowers -t PENDING | wc -l)
done

done
