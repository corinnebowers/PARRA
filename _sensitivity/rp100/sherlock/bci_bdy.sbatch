#!/bin/bash
#SBATCH --job-name=bci_bdy
#SBATCH --mail-type=ALL
#SBATCH --mail-user=<cbowers>@stanford.edu
#SBATCH --partition=cee
#SBATCH --time=1:00:00
#SBATCH --cpus-per-task=25
#SBATCH --mem=120G
#SBATCH --output=bci_bdy.log


# load modules
ml R/4 physics
ml proj gdal geos udunits

## execute script(s)
mkdir -p bci_bdy
mkdir -p manning
Rscript generate_files.R

