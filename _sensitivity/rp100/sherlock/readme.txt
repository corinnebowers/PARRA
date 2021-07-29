## how to generate LISFLOOD files for model fit & validation ##

1. run bci_bdy.sbatch 
  * generates Latin hypercube samples for parameters of interest (stored in samples_rp100.txt)
  * creates .bci and .bdy files for each sample (stored in bci_bdy folder)
  * creates .n.asc files for each sample (stored in manning folder)

2. run run_lisflood.sh
  * calls makepar.sh, which creates .par files for each sample (stored in parfiles folder)
  * runs LISFLOOD and creates inundation maps for each sample (stored in results folder)


