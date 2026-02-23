# BlueSCC

Calculating the Blue Social Cost of Carbon.

This repository runs on **R (R-4.4.2)** to calculate biophysical and economic climate damage functions for corals, mangroves, fisheries, mariculture and seaports.

The **Python (Python 3.14)** files in `Code/SCC/` convert those damage functions to files ready to plug into RICE50+.

The user needs **RICE50+** to calculate the Blue SCC: https://github.com/witch-team/RICE50xmodel

## Quick Start

**Automated workflow** (recommended):
```r
source("setup.r")  # Install packages
source("main.R")   # Run complete pipeline
```

**Manual workflow**: Navigate to `Code/` and run scripts in each module subfolder (Corals, Mangroves, Fisheries_and_Mariculture, Ports).

## Reproducing Paper Results

Once you have RICE50+ results, the figures in the paper are replicable by running code in `Code/MainText_Figures_Code/`.

Figures are saved to `Figures/Main/`.

For detailed RICE50+ integration steps, see `Code/SCC/SCC_steps.md`.

## Performance

Typical running time on a standard computer: **< 1 hour**

## Citation

This code reproduces results from:

**Bastien-Olvera, B., et al. (2025).** Accounting for ocean impacts nearly doubles the social cost of carbon. *Nature Climate Change*. DOI: [10.1038/s41558-025-02533-5](https://doi.org/10.1038/s41558-025-02533-5)
