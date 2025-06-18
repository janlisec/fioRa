.onLoad <- function(libname, pkgname) {
  # py_require() does not work at the BAM server, hence the manual solution using a conda environment
  #reticulate::py_require("git+https://github.com/BAMeScience/fiora.git")
  check_fiora_python_installation()
}
