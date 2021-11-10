#!/bin/bash
#SBATCH --job-name=generate_files_2
#SBATCH --mail-type=ALL
#SBATCH --mail-user=cbowers@stanford.edu
#SBATCH --partition=cee
#SBATCH --time=10:00
#SBATCH --cpus-per-task=4
#SBATCH --mem=10G
#SBATCH --output=generate_files_2.log

## load modules
ml R/4 physics
ml proj gdal geos udunits

## execute script(s)
mkdir -p files
mkdir -p files/bci files/bdy
Rscript generate_files_2.R

