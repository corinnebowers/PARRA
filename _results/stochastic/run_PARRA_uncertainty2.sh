#!/bin/sh

## clean up and rename files to match simulation indices
mkdir -p checkpoints
mkdir -p logfiles
for i in $(seq 1 1000) 
  do \
  mv checkpoints_$i/DV.Rdata checkpoints/DV_$i.Rdata 
  rm -r checkpoints_$i
done
rm *.log

## create id.txt, list of unused indices
ls checkpoints > files.txt
rm -f id.txt
touch id.txt
for i in $(seq 1 1000) 
do \
  var=$(grep DV_$i.Rdata files.txt)
  # echo $var >> id.txt
  if [ -z "$var" ]; then echo $i >> id.txt; fi
done
rm files.txt

## repeat simulations for unused indices
sbatch run_PARRA_uncertainty2.sbatch

## clean up again
for i in $(cat id.txt) 
  do \
  mv checkpoints_$i/DV.Rdata checkpoints/DV_$i.Rdata 
  rm -r checkpoints_$i
done
rm *.log