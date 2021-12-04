#!/bin/bash
#SBATCH --job-name=errors
#SBATCH --mail-type=ALL
#SBATCH --mail-user=cbowers@stanford.edu
#SBATCH --array=1-10
#SBATCH --partition=cee
#SBATCH --time=12:00:00
#SBATCH --mem=160G
#SBATCH --cpus-per-task=32
#SBATCH --output=logfiles/m%a.log

## load modules
ml R/4 physics
ml gdal geos proj udunits

## execute script(s)
mkdir -p results
srun Rscript calc_surrogate_errors.R $SLURM_ARRAY_TASK_ID
