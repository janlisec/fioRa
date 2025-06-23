
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{fioRa}`

<!-- badges: start -->

[![Static
Badge](https://img.shields.io/badge/LiveApp-blue)](https://apps.bam.de/shn01/fioRa/)
[![Static
Badge](https://img.shields.io/badge/doi-10.1038/s41467--025--57422--4-yellow.svg)](https://doi.org/10.1038/s41467-025-57422-4)
<!-- badges: end -->

**FIORA**is an in silico fragmentation algorithm designed to predict
tandem mass spectra (MS/MS) with high accuracy. Using a graph neural
network, FIORA models bond cleavages, fragment intensities, and
estimates retention times (RT) and collision cross sections (CCS).

The original model and prediction algorithm is implemented in Python and
can be found at its [GitHub Repo](https://github.com/BAMeScience/fiora).

The full description of the algorithm is published in a [Nature
Communications article](https://doi.org/10.1038/s41467-025-57422-4).

The R package `{fioRa}` provides a wrapper for **FIORA**, to either run
the Python script using the R function `run_script()` or start a GUI
(Shiny-App) using the R function `run_app()`.

## Installation

You can install the development version of `{fioRa}` from GitHub.

``` r
install.packages("devtools")
devtools::install_github("janlisec/fioRa")
```

Before first use you need to set up a python installation and a virtual
environment *fiora* which can be achieved from within R with the help of
the `reticulate` package.

``` r
# [1] install R package reticulate if you haven't done yet
install.packages("reticulate")

# [2] install python (miniconda) if you haven't done yet
if (!file.exists(reticulate::miniconda_path())) {
  reticulate::install_miniconda(path = reticulate::miniconda_path(), update = FALSE, force = FALSE)
}

# [3] install python module fiora in a virtual environment
if (!reticulate::virtualenv_exists("fiora")) {
  reticulate::virtualenv_create("fiora")
  reticulate::virtualenva_install(envname = "fiora", packages = "git+https://github.com/BAMeScience/fiora.git")
}
```

## Run

Now, you can launch the application as a Shiny-App by running:

``` r
fioRa::run_app()
```

<figure>
<img src="dev/fioRa_screenshot.png?raw=true" title="fioRa screenshot"
alt="fioRa screenshot" />
<figcaption aria-hidden="true">fioRa screenshot</figcaption>
</figure>

or use the exported function `run_script` to work in the R command line
directly:

``` r
fioRa::run_script(Name = "Example_0", SMILES = "CC1=CC(=O)OC2=CC(OS(O)(=O)=O)=CC=C12")
```

## About

You are reading the doc about version 0.2.4 compiled on 2025-06-23
13:41:29.69554.

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading fioRa
#> ── R CMD check results ──────────────────────────────────────── fioRa 0.2.4 ────
#> Duration: 2m 44.7s
#> 
#> ❯ checking for future file timestamps ... NOTE
#>   unable to verify current time
#> 
#> ❯ checking top-level files ... NOTE
#>   Non-standard file/directory found at top level:
#>     'app.R'
#> 
#> ❯ checking for detritus in the temp directory ... NOTE
#>   Found the following files/directories:
#>     '__pycache__'
#>     'torch_geometric.nn.conv.rgcn_conv_RGCNConv_propagate_xw60gt6g.py'
#> 
#> 0 errors ✔ | 0 warnings ✔ | 3 notes ✖
```

``` r
covr::package_coverage()
#> fioRa Coverage: 64.79%
#> R/run_app.R: 0.00%
#> R/app_utils.R: 22.50%
#> R/mod_page_fioRa.R: 73.94%
#> R/run_script.R: 75.00%
#> R/fct_read_fiora.R: 78.57%
#> R/app_config.R: 100.00%
#> R/app_server.R: 100.00%
#> R/app_ui.R: 100.00%
```
