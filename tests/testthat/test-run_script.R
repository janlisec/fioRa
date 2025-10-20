testthat::test_that("fioRa `run_script` function works with default parameters", {
  skip_if(Sys.getenv("CI") == "true", "Skipping test on CI")
  td <- fioRa::test_data
  x <- setNames(data.frame(
    t(sapply(td[2:11], function(x) { strsplit(x, ",")[[1]] }))),
    strsplit(td[1], ",")[[1]]
  )
  tmp <- run_script(x = x[1,,drop=FALSE])
  testthat::expect_length(tmp, 1)
  testthat::expect_length(tmp[[1]], 9)
  testthat::expect_true("spec" %in% names(tmp[[1]]))
  testthat::expect_true(nrow(tmp[[1]][["spec"]])==4)
})
