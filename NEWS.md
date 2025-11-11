# v0.3.5 by jan.lisec@bam.de

* CRAN badge
* modified module of selectInputWithButtons to be better tested

# v0.3.4 by jan.lisec@bam.de

* App GUI improved (better selection of individual results)
* default python environment switched to conda

# v0.3.3 by jan.lisec@bam.de

* `run_script` gains an argument `fmt` which allows to export results in a data frame
* additional test_that tests implemented to prepare for CRAN release

# v0.3.2 by jan.lisec@bam.de

* included `test_data` object in the package
* modified the `run_script()` function to accept a data frame with multiple 
  lines as input

# v0.3.1 by jan.lisec@bam.de

* updated to **fiora** model `1.0.1`
* increased resolution of spectra plots in app
* allowed to use different fiora models (from different python environments)
  in function *run_script()*

# v0.2.5 by jan.lisec@bam.de

* neutral losses shown in spectra plots
* sum formulas corrected for H number using the adduct annotation of **fiora**
* sum formulas scaled to comparable size in spectra plots

# v0.2.4 by jan.lisec@bam.de

* *predict()* renamed to *run_script()*
* detection of python and script path platform specific
* script evaluation platform specific

# v0.2.3 by jan.lisec@bam.de

* script path hardcoded; reticulate not used

# v0.2.2 by jan.lisec@bam.de

* moved reticulate to Suggest in Description if python installation and script is found

# v0.2.1 by jan.lisec@bam.de

* modified fiora installation using a conda environment

# v0.1.1 by jan.lisec@bam.de

* Shiny App working on shinyapps.io
* local function `predict` is working
* spectrum plot is showing formulas and molecular structures
* several testthat tests implemented
* ReadMe and News.md set up

# v0.0.0.9000 by jan.lisec@bam.de

* initial GitHub submission
