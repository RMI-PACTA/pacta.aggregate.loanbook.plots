
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pacta.supervisor.analysis

<!-- badges: start -->
<!-- badges: end -->

This repository is meant to host all scripts and R functions used to
generate all output used in PACTA 4 Banks projects run by bank
supervisors or similar institutions.

All third party data must be input by the user and is not part of this
repository.

File paths for input and output files are set up in the `.env` file as
explained below.

Scripts will be found at root level.

## dotenv

A file called `.env` must be created in the root of this directory. The
file should have the following variables:

``` bash
# I/O directories
DIR_SCENARIO="PATH/TO/SCENARIO/FOLDER"
DIR_ABCD="PATH/TO/ABCD/FOLDER"
DIR_RAW="PATH/TO/RAW_LOANBOOK/FOLDER"
DIR_MATCHED="PATH/TO/MATCHED_LOANBOOK/FOLDER"
DIR_OUTPUT="PATH/TO/OUTPUT/FOLDER"

# input file names
FILENAME_SCENARIO_TMS="scenario_tms.csv"
FILENAME_SCENARIO_SDA="scenario_sda.csv"
FILENAME_REGIONS_GECO_2022="region_geco2022.csv"
FILENAME_REGIONS_WEO_2022="region_weo2022.csv"
FILENAME_ABCD="abcd_data.csv"

# project parameters
PARAM_SCENARIO_SOURCE="weo_2022"
PARAM_SCENARIO_SELECT="nze_2050"
PARAM_REGION_SELECT="global"
# normally the start year should correspond with year of the publication of the
# scenario in use
PARAM_START_YEAR=2022
# regions must be available for the selected scenario
PARAM_BENCHMARK_REGIONS="global,european union"
```

## Installation

You can install the development version of pacta.supervisor.analysis
from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("RMI-PACTA/pacta.supervisor.analysis")
```
