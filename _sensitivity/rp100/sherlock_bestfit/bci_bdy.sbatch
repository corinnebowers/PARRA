#!/bin/bash
#SBATCH --job-name=bci_bdy
#SBATCH --mail-type=ALL
#SBATCH --mail-user=<cbowers>@stanford.edu
#SBATCH --partition=owners
#SBATCH --time=1:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=1G
#SBATCH --output=bci_bdy.log

# load modules
ml R/4 physics
ml proj gdal geos udunits

## execute script(s)
Rscript generate_files.R

