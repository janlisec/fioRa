
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{fioRa}`

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install the development version of `{fioRa}` like so:

``` r
install.packages("devtools")
devtools::install_github("janlisec/fioRa")
```

Before first use you need to set up a python installation and a virtual
environment `fiora` which can be achieved from within R with the help of
the `reticulate` package by:

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

You can launch the application as a Shiny-App by running:

``` r
fioRa::run_app()
```

or use the exported function `run_script` to work in R directly:

``` r
fioRa::run_script()
```

## About

You are reading the doc about version 0.2.3 compiled on 2025-06-20
14:09:47.933408.

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading fioRa
#> ── R CMD check results ──────────────────────────────────────── fioRa 0.2.3 ────
#> Duration: 3m 9.7s
#> 
#> ❯ checking for future file timestamps ... NOTE
#>   unable to verify current time
#> 
#> ❯ checking top-level files ... NOTE
#>   Non-standard file/directory found at top level:
#>     'app.R'
#> 
#> ❯ checking R code for possible problems ... NOTE
#>   square_subplot_coord: no visible global function definition for 'par'
#>   Undefined global functions or variables:
#>     par
#>   Consider adding
#>     importFrom("graphics", "par")
#>   to your NAMESPACE file.
#> 
#> ❯ checking for detritus in the temp directory ... NOTE
#>   Found the following files/directories:
#>     '__pycache__'
#>     'torch_geometric.nn.conv.rgcn_conv_RGCNConv_propagate_vebe80xu.py'
#> 
#> 0 errors ✔ | 0 warnings ✔ | 4 notes ✖
```

``` r
covr::package_coverage()
#> fioRa Coverage: 64.33%
#> R/run_app.R: 0.00%
#> R/app_utils.R: 23.60%
#> R/mod_page_fioRa.R: 77.27%
#> R/fct_read_fiora.R: 78.57%
#> R/run_script.R: 81.82%
#> R/app_config.R: 100.00%
#> R/app_server.R: 100.00%
#> R/app_ui.R: 100.00%
```
