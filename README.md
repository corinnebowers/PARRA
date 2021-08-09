# Performance-based Atmospheric River Analysis (PARRA) Framework

This repository provides supplementary information to the paper XX (doi:XX). It contains the code to implement the Performance-Based Atmospheric River Risk Analysis (PARRA) framework, which is illustrated in the following figure:

<p align="center">
  <img src="https://user-images.githubusercontent.com/49569602/128727103-e81cd681-d8bc-42f0-9b67-97f4d7eec394.png" width=75% height=75%>
</p>

This is Figure 1 from the paper ilustrating the theoretical outline of the PARRA framework, with a few additional annotations. The annotations represent R markdown files that provide additional context and replicate the figures from the paper. These five markdown files are described in more detail below.

* <a href="https://corinnebowers.github.io/sonoma.html">sonoma.Rmd</a>: Generates a figure of Sonoma County with rivers, creeks, and major cities/towns identified, as well as a figure inset of the state of California for regional context.
* <a href="https://corinnebowers.github.io/rp100.html">rp100.Rmd</a>: Performs sensitivity analysis and best-fit calibration for LISFLOOD environmental parameters (floodplain roughness, channel shape, etc.).
* surrogatemodel.Rmd: Performs best-fit calibration for surrogate model hyperparameters.
* componentmodels.Rmd: Steps through a model-by-model validation and case study comparison of a severe AR event occurring in 2019 in Sonoma County.
* lossexceedance.Rmd: Generates a first-of-its-kind loss exceedance curve for Sonoma County and calculates the expected benefit of a hypothetical community flood mitigation action.

For an even more detailed description, please visit the code implementation flowchart <a href = "https://www.corinnebowers.com/parra">here</a>. The flowchart explains the purpose and sequence of all files included in this repository, and attempts to provide a roadmap for users who want to replicate and build upon the component models in the PARRA framework. All questions can be directed to \[my email\]. 

notes to self: 

* INUN.R is nowhere on the flowchart
* on the flowchart: add explanatory text about the rollovers \& discourage mobile use
* figure out in Github how to open these links in separate tabs
* include a new .Rmd file that does component-level sensitivity analysis (i.e. PRCP contributes the most to the variability in the loss histogram)
