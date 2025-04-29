testthat::test_that("fioRa `predict` function works with default parameters", {
  tmp <- fioRa::predict(Name = "Example_0", SMILES = "CC1=CC(=O)OC2=CC(OS(O)(=O)=O)=CC=C12", Precursor_type = "[M-H]-", CE = 17, Instrument_type = "HCD")
  testthat::expect_length(tmp, 1)
  testthat::expect_length(tmp[[1]], 9)
  testthat::expect_true("spec" %in% names(tmp[[1]]))
  testthat::expect_true(nrow(tmp[[1]][["spec"]])==12)
})
