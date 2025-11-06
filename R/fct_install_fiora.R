#' @title Install the python module `fiora` into a conda environment.
#'
#' @description This function will check and perform the installation of three
#'     components in the following order: reticulate, miniconda and fiora. It
#'     will ensure that a working conda environment `fiora` is available. This
#'     is a prerequisite for both, \link{run_app} and \link{run_script}.
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
install_fiora <- function() {
  # [1] install R package reticulate if you haven't done yet
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    utils::install.packages("reticulate")
  }

  # [2] install python (miniconda) if you haven't done yet
  if (!file.exists(reticulate::miniconda_path())) {
    reticulate::install_miniconda(path = reticulate::miniconda_path(), update = FALSE, force = FALSE)
  }

  # [3] install python module fiora in a conda environment and activate
  if (!("fiora" %in% reticulate::conda_list()$name)) {
    reticulate::conda_create("fiora")
    reticulate::conda_install("fiora", packages = "git+https://github.com/BAMeScience/fiora.git", pip = TRUE)
    reticulate::use_condaenv("fiora", required = TRUE)
  }

  return(find_fiora_predict_paths(default_path = ""))
}
