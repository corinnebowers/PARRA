#!/bin/bash

# find the number of samples
n=$(wc -l samples_grid.txt | cut -f1 -d ' ')
let "n-=1"

for i in $(seq 1 $n)
do \
plusone=$(expr $i + 1)
line=$(tail -n+$plusone samples_grid.txt | head -n1)

# read parameters for the ith iteration
SGCn=0.0300
#SGCr=
#SGCp=

# create lisflood parfile
bash makepar.sh \
"gridded$i"			`#file name` \
"results" 			`#results directory` \
"5184000" 			`#simulation length (seconds)` \
"1" 				`#simulation timestep (seconds)` \
"russian.dem.asc" 		`#DEM raster file` \
"russian.width.asc"		`#channel width raster file` \
"${SGCn}" 			`#channel roughness coefficient` \
""		 		`#channel depth parameter, r` \
""				`#channel depth parameter, p` \
"bci_bdy/gridflow$i.bci" 	`#.bci file` \
"bci_bdy/gridflow$i.bdy" 	`#.bdy file` \
"" 				`#starting .wd file` \
"russian.n.asc"		 	`#spatially varying floodplain roughness raster` \
"" 				 #constant floodplain roughness coefficient

# calculate SGC files (once)
if [ $i == 1 ]; then echo "debug" >> gridded$i.par; fi

# submit lisflood job with sbatch wrap
sbatch \
--job-name=lf${i} \
--output=logfiles/gridded${i}.log \
--nodes=1 \
--ntasks=1 \
--cpus-per-task=20 \
--mem=250 \
--time=4:00:00 \
-p cee,owners \
--mail-type=ALL \
--mail-user=cbowers@stanford.edu \
--wrap="ml netcdf; lisflood -v gridded$i.par"

sleep 2

# wait for queue to clear a bit
jobs=$(squeue -u cbowers -t PENDING | wc -l)
while [ $jobs -gt 10 ]
do
  sleep 1m 
  jobs=$(squeue -u cbowers -t PENDING | wc -l)
done

done
