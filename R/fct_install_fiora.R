#' @title Install the python module `fiora` into a conda environment.
#'
#' @description This function will check and perform the installation of three
#'     components in the following order: reticulate, miniconda and fiora. It
#'     will ensure that a working conda environment `fiora` is available. This
#'     is a prerequisite for both, \link{run_app} and \link{run_script}.
#'
#' @param conda_name The name of the conda environment where fiora shall be installed to.
#'
#' @return A list providing the current OS and path information on the current
#'     python executable and the fiora script.
#'
#' @examples
#' \dontrun{
#'   # this will install packages and software on your machine
#'   install_fiora()
#' }
#'
#' @export
install_fiora <- function(conda_name = "fiora") {
  # [1] install R package reticulate if you haven't done yet
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    utils::install.packages("reticulate")
  }

  # [2] install python (miniconda) if you haven't done yet
  if (!file.exists(reticulate::miniconda_path())) {
    reticulate::install_miniconda(path = reticulate::miniconda_path(), update = FALSE, force = FALSE)
  }

  # [3] install python module fiora in a conda environment and activate
  if (conda_name %in% reticulate::conda_list()$name) {
    if (interactive()) {
      q <- utils::askYesNo(msg = paste("There is a conda environment of name '", conda_name, "' present. Shall this environment be replaced with the new installation of 'fiora'?"))
      if (q) {
        message("removing old environment ", conda_name)
        reticulate::conda_remove(conda_name)
        reticulate::conda_create(conda_name, channel = c("conda-forge"), forge = FALSE)
        message("installing fiora to ", conda_name)
        reticulate::conda_install(conda_name, packages = "git+https://github.com/BAMeScience/fiora.git", pip = TRUE)
        reticulate::use_condaenv(conda_name, required = TRUE)
      } else {
        message(conda_name, " was found in reticulate::conda_list()$name (", paste(reticulate::conda_list()$name, collapse=", "), ") and not overwritten.")
      }
    } else {
      return(find_fiora_predict_paths(default_path = "C:\\Users\\jlisec\\AppData\\Local\\r-miniconda"))
    }
  } else {
    reticulate::conda_create(conda_name, channel = c("conda-forge"), forge = FALSE)
    message("installing fiora to ", conda_name)
    reticulate::conda_install(conda_name, packages = "git+https://github.com/BAMeScience/fiora.git", pip = TRUE)
    reticulate::use_condaenv(conda_name, required = TRUE)
  }

  return(find_fiora_predict_paths(default_path = ""))
}
