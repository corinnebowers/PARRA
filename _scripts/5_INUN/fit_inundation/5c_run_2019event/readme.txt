1. Run generate_files.sbatch to create .bci, .bdy, .gauge, and .stage files based on the hydrograph timeseries recorded at USGS 11463500. 
2. Run run_lisflood.sh to 
	(a) generate .par files for the 2019 scenario event, and 
	(b) calculate LISFLOOD inundation maps for the 2019 scenario event.
3. Run cleanup_lisflood.sh to organize output files from step 3.

Note: step 2 is set up to run on the Stanford Sherlock computing cluster. This code can be modified to run on another similar HPC environment or on a personal computer.
