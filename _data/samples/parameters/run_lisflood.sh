#!/bin/bash

# find the number of samples
n=$(wc -l samples_grid.txt | cut -f1 -d ' ')
let "n-=1"

for i in $(seq 1 $n)
do \
# create lisflood parfile
bash makepar.sh \
"gridded$i" `#file name` \
"results" `#results directory` \
"3456000" `#simulation length (seconds)` \
"1" `#simulation timestep (seconds)` \
"russian.dem.asc" `#DEM raster file` \
"russian.width.asc"`#channel width raster file` \
"" `#channel roughness coefficient` \
"" `#channel depth parameter` \
"bci_bdy/gridflow${i}.bci" `#.bci file` \
"bci_bdy/gridflow${i}.bdy" `#.bdy file` \
"" `#starting .wd file` \
"russian.n.asc" `#spatially varying floodplain roughness raster` \
"" #constant floodplain roughness coefficient

# submit lisflood job with sbatch wrap
sbatch \
--job-name=lf${i} \
--output=logfiles/lisflood_${i}.log \
--nodes=1 \
--ntasks=1 \
--cpus-per-task=20 \
--mem=250 \
--time=6:00:00 \
-p cee \
--mail-type=ALL \
--mail-user=cbowers@stanford.edu \
--wrap="export OMP_NUM_THREADS=20; ml netcdf; lisflood -v gridded$i.par"

# add in a sleep so we don't overflow the scheduler
sleep 3

# wait for queue to clear a bit
jobs=$(squeue -u cbowers -p cee -t PENDING | wc -l)
while [ $jobs -gt 10 ]
do
  sleep 5m 
  jobs=$(squeue -u cbowers -p cee -t PENDING | wc -l)
done

done