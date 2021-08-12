#!/bin/bash
#SBATCH --job-name=array
#SBATCH --mail-type=ALL
#SBATCH --mail-user=cbowers@stanford.edu
#SBATCH --array=1,2,3,4,5,7,8,10
#SBATCH --partition=cee
#SBATCH --time=7-00:00:00
#SBATCH --mem=160G
#SBATCH --cpus-per-task=32
#SBATCH --output=m%a.log


# load modules
ml R/4
ml physics
ml gdal geos proj udunits

# execute script(s)
srun Rscript surrogatemodel_array.R $SLURM_ARRAY_TASK_ID
