#!/bin/bash
#SBATCH --job-name=event
#SBATCH --mail-type=ALL
#SBATCH --mail-user=cbowers@stanford.edu
#SBATCH --partition=cee,owners
#SBATCH --time=30:00
#SBATCH --mem=100G
#SBATCH --cpus-per-task=20
#SBATCH --output=PARRA.log

# load modules
ml R/4 gcc/10
ml physics
ml gdal geos proj udunits

# execute script(s)\
mkdir -p checkpoints
Rscript run_PARRA_event.R \
TRUE 1 `#AR` \
NA 1e4 `#PRCP` \
TRUE 1 `#HC` \
TRUE 1e4 `#RNFF` \
TRUE 1 `#HYDRO` \
TRUE 1 `#INUN` \
TRUE 1 `#DMG` \
TRUE 1 `#LOSS` \
"_results/prcpconst_smconst/checkpoints/"
