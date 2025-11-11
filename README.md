
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{fioRa}` 0.3.4

<!-- badges: start -->

[![Static
Badge](https://img.shields.io/badge/LiveApp-blue)](https://apps.bam.de/shn01/fioRa/)
[![CRAN
status](https://www.r-pkg.org/badges/version/fioRa)](https://CRAN.R-project.org/package=fioRa)
[![R-CMD-check](https://github.com/janlisec/fioRa/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/janlisec/fioRa/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/janlisec/fioRa/graph/badge.svg)](https://app.codecov.io/gh/janlisec/fioRa)
[![Static
Badge](https://img.shields.io/badge/doi-10.1038/s41467--025--57422--4-yellow.svg)](https://doi.org/10.1038/s41467-025-57422-4)
<!-- badges: end -->

**FIORA** is an in silico fragmentation algorithm designed to predict
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

Before first use you need to set up a python installation and a conda
environment *fiora* which can be achieved from within R with the help of
the `reticulate` package and a convenience function.

``` r
fioRa::install_fiora()
#> No valid 'default_path' provided, using reticulate::miniconda_path 'C:/Users/jlisec/AppData/Local/r-miniconda'.
#> $os
#> [1] "Windows"
#> 
#> $python
#> [1] "C:\\Users\\jlisec\\AppData\\Local\\r-miniconda\\envs\\fiora\\python.exe"
#> 
#> $script
#> [1] "C:\\Users\\jlisec\\AppData\\Local\\r-miniconda\\envs\\fiora\\Scripts\\fiora-predict"
```

## Run

Now, you can launch the application as a Shiny-App.

``` r
fioRa::run_app()
```

<figure>
<img src="dev/fioRa_screenshot.png?raw=true" title="fioRa screenshot"
alt="fioRa screenshot" />
<figcaption aria-hidden="true">fioRa screenshot</figcaption>
</figure>

Alternatively you can use the exported function `run_script()` to work
in the R command line directly. This will accept R styled input
parameters, generate an appropriate temporary **FIORA** input file,
process it and return an R styled list including the predicted MS/MS
spectrum.

``` r
x <- data.frame(
  Name = "Example_0",
  SMILES = "CC1=CC(=O)OC2=CC(OS(O)(=O)=O)=CC=C12",
  Precursor_type = "[M-H]-",
  CE = 17,
  Instrument_type = "HCD"
)
fioRa::run_script(x = x, annotation = TRUE)
#> No valid 'default_path' provided, using reticulate::miniconda_path 'C:/Users/jlisec/AppData/Local/r-miniconda'.
#> $Example_0
#> $Example_0$TITLE
#> [1] "Example_0"
#> 
#> $Example_0$SMILES
#> [1] "CC1=CC(=O)OC2=CC(OS(O)(=O)=O)=CC=C12"
#> 
#> $Example_0$FORMULA
#> [1] "C10H8O6S"
#> 
#> $Example_0$PRECURSOR_MZ
#> [1] "254.99688252391005"
#> 
#> $Example_0$PRECURSORTYPE
#> [1] "[M-H]-"
#> 
#> $Example_0$COLLISIONENERGY
#> [1] "17.0"
#> 
#> $Example_0$INSTRUMENTTYPE
#> [1] "HCD"
#> 
#> $Example_0$COMMENT
#> [1] "\"In silico generated spectrum by FIORA OS v1.0.0\""
#> 
#> $Example_0$spec
#>          mz         int                           SMILES  adduct  formula
#> 1  78.94844 0.013833499                      O=[SH](=O)O [M-3H]-    H2O3S
#> 2  79.95681 0.002825602                      O=[SH](=O)O [M-2H]-    H2O3S
#> 3 175.03897 0.899893463           Cc1cc(=O)oc2cc(O)ccc12  [M-H]-  C10H8O3
#> 4 254.99688 0.102497801 Cc1cc(=O)oc2cc(OS(=O)(=O)O)ccc12  [M-H]- C10H8O6S
```

## About

You are reading the doc about version 0.3.4 compiled on 2025-11-11
14:48:03.385989.
