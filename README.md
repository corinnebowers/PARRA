# Performance-based Atmospheric River Analysis (PARRA) Framework

This repository provides supplementary information to the paper XX (doi:XX). It contains the code to implement the Performance-Based Atmospheric River Risk Analysis (PARRA) framework, which is illustrated in the following figure:

<p align="center">
  <img src="https://user-images.githubusercontent.com/49569602/128727103-e81cd681-d8bc-42f0-9b67-97f4d7eec394.png" width=75% height=75%>
</p>

This is Figure 1 from the paper ilustrating the theoretical outline of the PARRA framework, with a few additional annotations. The annotations represent R markdown files that provide additional context and replicate the figures from the paper. These five files are described in more detail below.

* <a href="https://corinnebowers.github.io/sonoma.html">sonoma.Rmd</a>: generates a figure of Sonoam County with rivers, creeks, and major cities/towns identified, as well as a figure inset of California for context
* <a href="https://corinnebowers.github.io/rp100.html">rp100.Rmd</a>: performs sensitivity analysis and best-fit calibration for LISFLOOD environmental parameters (floodplain roughness, channel shape, etc.) 
* surrogatemodel.Rmd:
* componentmodels.Rmd:
* lossexceedance.Rmd:

For an even more detailed description, please visit the code implementation flowchart <a href = "https://www.corinnebowers.com/parra">here</a>. The flowchart explains the purpose and sequence of all files included in this repository, and attempts to provide a roadmap for users who want to replicate and build upon the component models in the PARRA framework.
