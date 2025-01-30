# EUcityProj

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14004322.svg)](https://doi.org/10.5281/zenodo.14004322)

Fully replicable code performing the health impact projections reported in the paper

> Masselot, P., Mistry, M.N., Rao, S. *et al*. Estimating future heat-related and cold-related mortality under climate change, demographic and adaptation scenarios in 854 European cities. *Nat Med* (2025). [https://doi.org/10.1038/s41591-024-03452-2](https://doi.org/10.1038/s41591-024-03452-2)

Results from the paper can be explroed interactively in a dedicated [Shiny app](https://ehm-lab.shinyapps.io/vistemphip/).

> [!WARNING]
> Reproducing the full results in extremely intensive computationally and memory wise. It is recommended to test the scripts on a reduced number of cities, and with a reduced number of simulations (see the `nsim` parameter). Also consider changing the parameters `ncores` and `grpsize` that also control the speed and memory efficiency of the analysis. See script `01_pkg_params.R`

## Data

The input data necessary to run the analysis are stored on a dedicated [Zenodo repository](https://doi.org/10.5281/zenodo.14004322). Download the `data.zip` archive and extract the files into a `data` folder in the project directory. this can be performed directly from R (see the first script `01_pkg_params.R`).

Tables and plots can be replicated without running the analysis by downloading and extracting the files in `results_parquet` found on the [Zenodo repository](https://doi.org/10.5281/zenodo.14004322). See additional details below

## Scripts

The scripts are run in order. Scripts 01 to 03 run the main analysis with results saved at the end of `03_attribution.R`. Scripts 04 to 06 can then be ran by loading results saved beforehand.

| Script | Descriptions |
| :--- | :--- |
| `01_pkg_params.R` | Load the necessary R libraries and defines all the analysis parameters. Also contains some parameters related to parallelization of the main loop which should be considered carefully before running a full analysis |
| `02_prep_data.R` | Load and prepare data about cities, demographic projections, and global warming levels |
| `03_attribution.R` | Centrepiece of the analysis. Loops through cities and scenarios to perform the health impact projections. For each city, the code loads temperature series and exposure-response function data, and compute the health impact of temperature. Health impacts are then summarised and saved in a temporary folder. Results are then aggregated once the health impact for all the cities from a country or regions have been computed. Note that the loop is stratified into chunks to avoid storing too much intermediary data simulataneously. 
| `04_tables.R` | Produces the main table from the article. Note that this script will load previously saved results and can then be used without having to run the previous scripts.
| `05_plots.R` | Produces plots featured in the main text of the article.
| `06_plot_supp.R` | Produces Extended Data plots.
