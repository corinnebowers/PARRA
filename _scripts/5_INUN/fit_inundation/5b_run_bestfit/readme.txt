1. Input best-fit parameters from 5a_fit_lisflood in the appropriate section in generate_files.R.
2. Run generate_files.sbatch to create .bci, .bdy, and .n files based on those best-fit parameter values. 
3. Run run_lisflood.sh to 
	(a) generate .par files for the best-fit parameter values, and 
	(b) calculate LISFLOOD inundation maps for the best-fit parameter values.
4. Run cleanup_lisflood.sh to organize output files from step 3.
5. Run report_accuracy.Rmd to calculate accuracy metrics for the best-fit LISFLOOD inundation map as compared against the FEMA NFHL. 

Note: steps 2 and 3 are set up to run on the Stanford Sherlock computing cluster. This code can be modified to run on another similar HPC environment or on a personal computer.
