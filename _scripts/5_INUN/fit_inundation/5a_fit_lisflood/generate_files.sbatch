#!/bin/bash
#SBATCH --job-name=generate_files
#SBATCH --mail-type=ALL
#SBATCH --mail-user=cbowers@stanford.edu
#SBATCH --partition=cee
#SBATCH --time=30:00
#SBATCH --cpus-per-task=32
#SBATCH --mem=80G
#SBATCH --output=generate_files.log

## load modules
ml R/4 physics
ml proj gdal geos udunits

## execute script(s)
mkdir -p files
mkdir -p files/bci files/bdy files/lulc
Rscript generate_files.R

