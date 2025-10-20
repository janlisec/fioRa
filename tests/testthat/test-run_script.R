testthat::test_that("fioRa `run_script` function works with default parameters", {
  skip_if(Sys.getenv("CI") == "true", "Skipping test on CI")
  tmp <- fioRa::run_script(Name = "Example_0", SMILES = "CC1=CC(=O)OC2=CC(OS(O)(=O)=O)=CC=C12", Precursor_type = "[M-H]-", CE = 17, Instrument_type = "HCD")
  testthat::expect_length(tmp, 1)
  testthat::expect_length(tmp[[1]], 9)
  testthat::expect_true("spec" %in% names(tmp[[1]]))
  testthat::expect_true(nrow(tmp[[1]][["spec"]])==4)
})
