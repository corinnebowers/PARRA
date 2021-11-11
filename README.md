# Performance-based Atmospheric River Analysis (PARRA) Framework

This repository provides supplementary information to the paper "A Performance-Based Approach to Quantify Atmospheric River Flood Risk" (doi:XX). It contains the code to implement the Performance-Based Atmospheric River Risk Analysis (PARRA) framework, which is illustrated in the following figure:

<!-- <p align="center">
  <img src="https://user-images.githubusercontent.com/49569602/128727103-e81cd681-d8bc-42f0-9b67-97f4d7eec394.png" width=75% height=75%>
</p> -->

<p align="center">
  <img src="https://user-images.githubusercontent.com/49569602/141232598-58d62a27-bd04-4d48-a49d-76c8fe636796.png" width=75% height=75%>
</p>

This is Figure 1 from the paper ilustrating the theoretical outline of the PARRA framework, with a few additional annotations. The annotations represent R markdown files that provide additional context and replicate the figures from the paper. These five markdown files are described in more detail below.

* <a href="https://corinnebowers.github.io/sonoma.html">sonoma.Rmd</a>: Generates a figure of Sonoma County with rivers, creeks, and major cities/towns identified, as well as a figure inset of the state of California for regional context.
* <a href="https://corinnebowers.github.io/rp100.html">rp100.Rmd</a>: Performs sensitivity analysis and best-fit calibration for LISFLOOD environmental parameters (floodplain roughness, channel shape, etc.).
* surrogatemodel.Rmd: Performs best-fit calibration for surrogate model hyperparameters.
* componentmodels.Rmd: Steps through a model-by-model validation and case study comparison of a severe AR event occurring in 2019 in Sonoma County.
* lossexceedance.Rmd: Generates a first-of-its-kind loss exceedance curve for Sonoma County and calculates the expected benefit of a hypothetical community flood mitigation action.

The purpose of these files is to...
