testthat::test_that("fioRa `run_script` function works with default parameters", {
  #testthat::skip_on_ci()
  testthat::skip_on_cran()
  td <- fioRa::test_data
  x <- stats::setNames(data.frame(
    t(sapply(td[2:11], function(x) { strsplit(x, ",")[[1]] }))),
    strsplit(td[1], ",")[[1]]
  )
  tmp <- suppressMessages(run_script(x = x[1,,drop=FALSE]))
  testthat::expect_length(tmp, 1)
  testthat::expect_length(tmp[[1]], 9)
  testthat::expect_true("spec" %in% names(tmp[[1]]))
  testthat::expect_true(nrow(tmp[[1]][["spec"]])==4)
})

testthat::test_that("fioRa `run_script` function returns annotated spectra output", {
  #testthat::skip_on_ci()
  testthat::skip_on_cran()
  td <- fioRa::test_data
  x <- setNames(data.frame(
    t(sapply(td[2:11], function(x) { strsplit(x, ",")[[1]] }))),
    strsplit(td[1], ",")[[1]]
  )
  tmp <- suppressMessages(run_script(x = x, annotation = TRUE))
  testthat::expect_length(tmp, 10)
  testthat::expect_length(tmp[[1]], 9)
  testthat::expect_true("spec" %in% names(tmp[[1]]))
  testthat::expect_true(nrow(tmp[[1]][["spec"]])==4)
  testthat::expect_true(ncol(tmp[[1]][["spec"]])==5)
  testthat::expect_true(all(c("SMILES","adduct","formula") %in% colnames(tmp[[1]][["spec"]])))

})
