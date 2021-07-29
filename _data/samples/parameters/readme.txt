how to generate the lookup database for the surrogate model:

1. generate dem, width, & n files
2. run the bci_bdy.script
    -generates 5,000 Latin hypercube samples of Qp & tp 
    -stores them as the samples_grid.txt file
    -creates .bci & .bdy files for each of the samples in samples_grid.txt
3. run LISFLOOD using the run_lisflood.sh script
    -generates 5,000 .par files using the makepar.sh script
