#!/bin/bash
#SBATCH --job-name=uncertainty
#SBATCH --mail-type=ALL
#SBATCH --mail-user=cbowers@stanford.edu
#SBATCH --array=1-1000
#SBATCH --partition=cee,owners
#SBATCH --time=60:00
#SBATCH --mem=100G
#SBATCH --cpus-per-task=20
#SBATCH --output=PARRA_%a.log

# load modules
ml R/4 physics
ml gdal geos proj udunits

# execute script(s)
mkdir -p checkpoints_$SLURM_ARRAY_TASK_ID
Rscript run_PARRA_uncertainty.R \
TRUE 1 `#AR` \
TRUE 1e2 `#PRCP` \
TRUE 1 `#HC` \
TRUE 1 `#RNFF` \
TRUE 1 `#HYDRO` \
TRUE 1 `#INUN` \
TRUE 1 `#DMG` \
TRUE 1 `#LOSS` \
"_results/stochastic_uncertainty/checkpoints_$SLURM_ARRAY_TASK_ID/"
