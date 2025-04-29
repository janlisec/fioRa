testthat::test_that("fioRa can read the standard fiora example output mgf", {
  fl <- system.file("extdata/expected_output.mgf", package = "fioRa")
  tmp <- fioRa:::read_fiora(fl = fl)
  # return value is a list
  testthat::expect_true(is.list(tmp))
  # return value is of length=10
  testthat::expect_length(tmp, 10)
  # list elements are named
  testthat::expect_false(is.null(names(tmp)))
  # all list elements contain element "spec"
  testthat::expect_true(all(sapply(tmp, function(x) {"spec" %in% names(x)})))

})
