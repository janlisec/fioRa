
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

## Run

You can launch the application as a Shiny-App by running:

``` r
fioRa::run_app()
```

or use the exported function `predict` to work in R directly:

``` r
fioRa::predict()
```

## About

You are reading the doc about version 0.0.0.9000 compiled on 2025-04-28
16:49:55.080425.

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading fioRa
#> ── R CMD check results ─────────────────────────────────── fioRa 0.0.0.9000 ────
#> Duration: 1m 47s
#> 
#> ❯ checking for detritus in the temp directory ... NOTE
#>   Found the following files/directories:
#>     '__pycache__'
#>     'torch_geometric.nn.conv.rgcn_conv_RGCNConv_propagate_9iq6fkmj.py'
#>     'torch_geometric.nn.conv.rgcn_conv_RGCNConv_propagate_nfx4yk0l.py'
#> 
#> 0 errors ✔ | 0 warnings ✔ | 1 note ✖
```

``` r
covr::package_coverage()
#> fioRa Coverage: 77.83%
#> R/run_app.R: 0.00%
#> R/mod_page_fioRa.R: 68.91%
#> R/fct_predict.R: 90.00%
#> R/app_config.R: 100.00%
#> R/app_server.R: 100.00%
#> R/app_ui.R: 100.00%
#> R/fct_read_fiora.R: 100.00%
```
