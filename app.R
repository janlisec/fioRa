# Launch the ShinyApp (Do not remove this comment)
# Or use the blue button on top of this file

# To update renv:
# renv::update()
# renv::snapshot()
# renv::install("janlisec/fioRa")

# To deploy to shinyapps.io:
# This app should not be deployed to shinyapps.io as it requires a python conda environment which takes very long to install using reticulate on shinyapps.io

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)
run_app() # add parameters here (if any)
