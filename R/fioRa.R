.onLoad <- function(libname, pkgname) {
  reticulate::py_require("git+https://github.com/BAMeScience/fiora.git")
}
