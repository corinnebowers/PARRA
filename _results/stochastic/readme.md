# Probabilistic Loss Generation

This folder contains the results of probabilistic loss calculations for
the study area, as explained in the `lossexceedance.Rmd` markdown file.
Each calculation generates a 3,200-year synthetic catalog of AR events.
The loss calculation was repeated 1,000 times to bootstrap a confidence
interval for the average annual loss (AAL).

The loss calculations were performed using Sherlock, Stanford’s
high-performance computing cluster. An outline of that process is
enumerated below.

1.  Run `run_PARRA_uncertainty.sbatch` to generate 1,000 loss
    calculations.
2.  Run `run_PARRA_uncertainty2.sh` to clean up the output files and
    re-initialize loss calculations for any runs that errored out.
    Repeat as many times as necessary.
