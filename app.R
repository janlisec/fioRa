# Launch the ShinyApp (Do not remove this comment)
# Or use the blue button on top of this file

# To update renv:
# renv::update()
# renv::snapshot()
# renv::install("janlisec/eCerto")

# To deploy to shinyapps.io:
# rsconnect::deployApp(appDir = "C:/Users/jlisec/Documents/Rpackages/Rpackage_fioRa/fioRa", appName = "fioraApp", forceUpdate = TRUE)

# To check for errors in shinyapps.io:
# rsconnect::showLogs(appName = "fioraApp", account = "jali")


pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)
run_app() # add parameters here (if any)
