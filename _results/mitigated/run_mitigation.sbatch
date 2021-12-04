#!/bin/bash
#SBATCH --job-name=mitigation
#SBATCH --mail-type=ALL
#SBATCH --mail-user=cbowers@stanford.edu
#SBATCH --partition=cee
#SBATCH --time=2:00:00
#SBATCH --mem=100G
#SBATCH --cpus-per-task=12
#SBATCH --output=run_mitigation.log

# load modules
ml R/4 gcc/10
ml physics
ml gdal geos proj udunits

# execute script(s)
mkdir -p checkpoints results
Rscript run_mitigation.R 
