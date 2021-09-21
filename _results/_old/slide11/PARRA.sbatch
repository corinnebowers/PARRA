#!/bin/bash
#SBATCH --job-name=PARRA
#SBATCH --mail-type=ALL
#SBATCH --mail-user=cbowers@stanford.edu
#SBATCH --partition=cee,owners
#SBATCH --time=10:00
#SBATCH --mem=4G
#SBATCH --cpus-per-task=1
#SBATCH --output=PARRA_stochastic.log

# load modules
ml R/4 gcc/10
ml physics
ml gdal geos proj udunits

# execute script(s)\
mkdir -p _results/stochastic
Rscript _scripts/PARRA.R \
NA 1 `#AR` \
NA 1e2 `#PRCP` \
NA 1 `#HC` \
NA 1 `#RNFF` \
NA 1 `#HYDRO` \
NA 1 `#INUN` \
TRUE 1 `#DMG` \
TRUE 1 `#LOSS` \
"./_results/stochastic/"
