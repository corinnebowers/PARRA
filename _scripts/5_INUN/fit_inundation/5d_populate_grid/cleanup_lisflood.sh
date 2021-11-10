#!/bin/bash

## move .par files to folder
mkdir -p files/par
mv *.par files/par

## organize results files
cd results
mkdir -p mass max mxe tm
mv *.mass mass
mv *.max max
mv *.mxe mxe
mv *.*tm tm
rm *.dem 

## zip .max files
cd max
zip maxes.zip *
