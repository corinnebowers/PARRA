#!/bin/bash
#SBATCH --job-name=stoch2
#SBATCH --mail-type=ALL
#SBATCH --mail-user=cbowers@stanford.edu
#SBATCH --array=1-51
#SBATCH --partition=cee,owners
#SBATCH --time=60:00
#SBATCH --mem=100G
#SBATCH --cpus-per-task=20
#SBATCH --output=PARRA_%a.log

# load modules
ml R/4 physics
ml gdal geos proj udunits

# define which indices need to be run
id=$(cat id.txt | head -n$SLURM_ARRAY_TASK_ID | tail -n1)

# execute script(s)
mkdir -p checkpoints_$id
Rscript run_PARRA_uncertainty.R \
TRUE 1 `#AR` \
TRUE 1e2 `#PRCP` \
TRUE 1 `#HC` \
TRUE 1 `#RNFF` \
TRUE 1 `#HYDRO` \
TRUE 1 `#INUN` \
TRUE 1 `#DMG` \
TRUE 1 `#LOSS` \
"_results/stochastic_uncertainty/checkpoints_$id/"
