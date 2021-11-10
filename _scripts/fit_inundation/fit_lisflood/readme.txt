1. Input user-defined parameters in the appropriate section in generate_files.R.
2. Run generate_files.R to 
	(a) randomly generate samples of LISFLOOD parameter values using Latin hypercube sampling (LHS), and 
	(b) create .bci, .bdy, and .n files based on those random parameter values for each sample index. 
3. Run run_lisflood.sh to 
	(a) generate .par files for each sample index, and 
	(b) calculate LISFLOOD inundation maps for each sample index.


Note: step 3 must be completed on the Stanford Sherlock computing cluster or another similar HPC environment.