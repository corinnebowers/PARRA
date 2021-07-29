#!/bin/bash
#SBATCH --job-name=bci_bdy
#SBATCH --mail-type=ALL
#SBATCH --mail-user=<cbowers>@stanford.edu
#SBATCH --partition=cee
#SBATCH --time=1:00:00
#SBATCH --mem=500
#SBATCH --output=bci_bdy.log


# load modules
ml R/4

# execute script(s)
mkdir -p bci_bdy
Rscript generate_bdy.R
