# Performance-based Atmospheric River Analysis (PARRA) Framework

This repository provides supplementary information to the paper "A Performance-Based Approach to Quantify Atmospheric River Flood Risk" (https://doi.org/10.5194/nhess-2021-337, in review). 
It contains the code to implement the Performance-Based Atmospheric River Risk Analysis (PARRA) framework, which is illustrated in Figure 1 from the paper:

<!-- <p align="center">
  <img src="https://user-images.githubusercontent.com/49569602/128727103-e81cd681-d8bc-42f0-9b67-97f4d7eec394.png" width=75% height=75%>
</p> -->

<p align="center">
  <img src="https://user-images.githubusercontent.com/49569602/141232598-58d62a27-bd04-4d48-a49d-76c8fe636796.png" width=75% height=75%>
</p>

The code provided here was used to implement the PARRA framework for a case study application along the lower Russian River in Sonoma County, California. 
The `_data` folder contains tables and dataframes that support calculations throughout the modeling process.
The `_scripts` folder contains scripts to (a) fit component model implementations for each of the pinch point variables and (b) generate new realizations of pinch point variables. 
Each of the subfolders represents one of the pinch point variables, or the arrows in Figure 1 above.
The `_results` folder contains code and markdown files that reproduce key results from the paper. The markdown files are also available as interactive HTML documents through the links below.

* [demonstration.Rmd](https://corinnebowers.github.io/demonstration.html) walks through most of the case study demonstration presented in Section 3 of the paper, which compares observed vs. simulated values for each component model and presents results from recreating a severe 2019 atmospheric river event. This file reproduces Figures 2 through 8 from the paper.
* [lossexceedance.Rmd](https://corinnebowers.github.io/lossexceedance.html) presents results from running the entire framework in sequence and reproduces Figures 9 and 10 from the paper. 

If you have any further questions please contact Corinne Bowers. 
