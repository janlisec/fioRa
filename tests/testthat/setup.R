# this function ensures that the python temp dir is cleaned up after devtools::check
clean_python_tmp_dir <- function() {
  if (requireNamespace("reticulate", quietly = TRUE)) {
    python_temp_dir <- dirname(reticulate::py_run_string(
      "import tempfile; x=tempfile.NamedTemporaryFile().name",
      local = TRUE
    )$x)
    detritus <- dir(
      path = python_temp_dir,
      pattern = "torch_geometric|__pycache__",
      full.names = TRUE
    )
    #message("Cleaning up detritus: ", paste(basename(detritus), collapse=", "))
    unlink(detritus, force = TRUE)
  }
}

withr::defer(clean_python_tmp_dir(), testthat::teardown_env())
