1. Input best-fit parameters from 5a_fit_lisflood in the appropriate sections in generate_files.R (m, LULC) and run_lisflood.sh (SGCn, SGCr, SGCp).
2. Run generate_files.sbatch to 
	(a) generate random samples of hydrograph parameters (Qp & tp) using Latin hypercube sampling (LHS), and 
	(b) create .bci and .bdy files based on those hydrograph parameters for each sample index. 
3. Run run_lisflood.sh to generate .par files and calculate LISFLOOD inundation maps for each sample index.
4. Run cleanup_lisflood.sh to organize LISFLOOD output files.
5. Run generate_files_2.sbatch to identify failed LISFLOOD model runs (either did not finish or did not reach the ocean) and recalculate .bci and .bdy files for these indices.
6. Run run_lisflood_2.sh to regenerate .par files and recalculate LISFLOOD inundation maps for the failed model runs.
7. Iterate steps 4-6 until the number of failed model runs is acceptable, i.e. less than 5% of all indices. 

Note: steps 2, 3, 5, and 6 are set up to run on the Stanford Sherlock computing cluster. This code can be modified to run on another similar HPC environment or on a personal computer.
