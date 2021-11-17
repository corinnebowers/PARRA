# 5c) Generate LISFLOOD inundation map for 2019 scenario event

Here we use the best-fit site condition parameters determined in step
(a), i.e. channel parameters and floodplain roughness parameters, along
with the real hydrograph timeseries from the 2019 event, to generate a
LISFLOOD map of estimated inundation. This 2019 event inundation map
will be used as part of the comparison exercise for the inundation
component model and is included as Figure 7 in the paper.

This process was completed using Sherlock, Stanford’s high-performance
computing cluster. An outline of this process is enumerated below.

1.  Input best-fit parameters from step (a) in the appropriate sections
    in `run_lisflood.sh` (SGCn, SGCr, SGCp).
2.  Run `generate_files.sh` to create .bci, .bdy, .gauge, and .stage
    files based on the hydrograph timeseries recorded at USGS 11463500.
3.  Run `run_lisflood.sh` to generate the .par file and calculate
    LISFLOOD inundation map for the 2019 scenario event.
4.  Run `cleanup_lisflood.sh` to organize LISFLOOD output files.
