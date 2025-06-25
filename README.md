
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{fioRa}` 0.2.5

<!-- badges: start -->

[![Static
Badge](https://img.shields.io/badge/LiveApp-blue)](https://apps.bam.de/shn01/fioRa/)
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
  reticulate::virtualenv_install(envname = "fiora", packages = "git+https://github.com/BAMeScience/fiora.git")
}
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
fioRa::run_script(Name = "Example_0", SMILES = "CC1=CC(=O)OC2=CC(OS(O)(=O)=O)=CC=C12")
#> No valid default_path provided, using reticulate::miniconda_path.
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
#> [1] "\"In silico generated spectrum by Fiora OS v0.1.0\""
#> 
#> $Example_0$spec
#>           mz         int
#> 1   78.94844 0.001667393
#> 2   79.95681 0.022089725
#> 3   80.96409 0.028634388
#> 4   81.97246 0.002399962
#> 5  159.04406 0.002223121
#> 6  173.02332 0.002690818
#> 7  174.03169 0.004479981
#> 8  175.03897 0.394105196
#> 9  176.04734 0.003682357
#> 10 177.05462 0.025992798
#> 11 239.97286 0.004418257
#> 12 254.99688 0.494040728
```

## About

You are reading the doc about version 0.2.5 compiled on 2025-06-25
09:43:37.961743.
