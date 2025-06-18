#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # ensure that reticulate is set up and that fiora is installed
  waiter::waiter_show(html = "fioRa model and python environment are getting prepared...")
    ## previous version using py_require()
    #msg <- reticulate::py_require("git+https://github.com/BAMeScience/fiora.git")
    #if (is.null(msg)) message("firoa is ready")
    ## new version using conda environment
    check_fiora_python_installation()
  waiter::waiter_hide()
  # Your application server logic
  page_fioRa_server(id = "fioRa")
}
