#!/bin/bash

## load modules
ml R/4 physics
ml proj gdal geos udunits

## execute script(s)
mkdir -p files
Rscript generate_files.R
