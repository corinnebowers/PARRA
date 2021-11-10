1. Input best-fit parameters from 5a_fit_lisflood in the appropriate sections in run_lisflood.sh (SGCn, SGCr, SGCp).
2. Run generate_files.sbatch to create .bci, .bdy, .gauge, and .stage files based on the hydrograph timeseries recorded at USGS 11463500. 
3. Run run_lisflood.sh to generate .par file and calculate LISFLOOD inundation map for the 2019 scenario event.
4. Run cleanup_lisflood.sh to organize LISFLOOD output files.

Note: steps 2 and 3 are set up to run on the Stanford Sherlock computing cluster. This code can be modified to run on another similar HPC environment or on a personal computer.
