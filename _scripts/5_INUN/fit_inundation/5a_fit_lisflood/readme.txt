1. Input user-defined parameters in the appropriate section in generate_files.R.
2. Run generate_files.sbatch to 
	(a) generate random samples of LISFLOOD parameter values using Latin hypercube sampling (LHS), and 
	(b) create .bci, .bdy, and .n files based on those random parameter values for each sample index. 
3. Run run_lisflood.sh to generate .par files and calculate LISFLOOD inundation maps for each sample index.
4. Run cleanup_lisflood.sh to organize LISFLOOD output files.
5. Run fit_lisflood.Rmd to determine best-fit parameter values.
5. Manually update the best-fit parameter values in run_bestfit.sh.
6. Run run_bestfit.sh to generate a LISFLOOD inundation map using the bestfit parameters.
7. Run report_accuracy.Rmd to calculate accuracy metrics for the best-fit LISFLOOD inundation map as compared against the FEMA NFHL. 

Note: steps 2, 3, and 6 are set up to run on the Stanford Sherlock computing cluster. This code can be modified to run on another similar HPC environment or on a personal computer.
