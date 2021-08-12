#!/bin/bash

# find the number of samples
n=$(wc -l samples_grid.txt | cut -f1 -d ' ')
let "n-=1"

for i in $(seq 6 500)
do \
# create lisflood parfile
bash makepar.sh \
"gridded$i"		`#file name` \
"results" 		`#results directory` \
"3888000" 		`#simulation length (seconds)` \
"1" 			`#simulation timestep (seconds)` \
"russian.dem.asc" 	`#DEM raster file` \
"russian.width.asc"	`#channel width raster file` \
"0.01800" 		`#channel roughness coefficient` \
"0.47750"	 	`#channel depth parameter, r` \
""			`#channel depth parameter, p` \
"bci_bdy/gridflow$i.bci"	`#.bci file` \
"bci_bdy/gridflow$i.bdy"	`#.bdy file` \
"" 			`#starting .wd file` \
"russian.n.asc" 	`#spatially varying floodplain roughness raster` \
"" 			 #constant floodplain roughness coefficient

# submit lisflood job with sbatch wrap
sbatch \
--job-name=lf${i} \
--output=logfiles/gridded${i}.log \
--nodes=1 \
--ntasks=1 \
--cpus-per-task=20 \
--mem=250 \
--time=4:00:00 \
-p cee \
--mail-type=ALL \
--mail-user=cbowers@stanford.edu \
--wrap="ml netcdf; lisflood -v gridded$i.par"

sleep 2

# wait for queue to clear a bit
jobs=$(squeue -u cbowers -p cee -t PENDING | wc -l)
while [ $jobs -gt 10 ]
do
  sleep 10m 
  jobs=$(squeue -u cbowers -p cee -t PENDING | wc -l)
done

done
