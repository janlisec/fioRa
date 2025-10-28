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

testthat::test_that("fioRa can read the standard fiora example annotated output mgf", {
  fl <- system.file("extdata/annotated_output.mgf", package = "fioRa")
  tmp <- fioRa:::read_fiora(fl = fl)
  # return value is a list
  testthat::expect_true(is.list(tmp))
  # return value is of length=10
  testthat::expect_length(tmp, 10)
  # list elements are named
  testthat::expect_false(is.null(names(tmp)))
  # all list elements contain element "spec"
  testthat::expect_true(all(sapply(tmp, function(x) {"spec" %in% names(x)})))
  # all "spec" elements contain adduct and formula columns
  testthat::expect_true(all(sapply(tmp, function(x) { all(c("adduct", "formula") %in% colnames(x[["spec"]])) })))
})

testthat::test_that("fioRa can read the standard fiora example annotated output mgf and combine output into data frame", {
  fl <- system.file("extdata/annotated_output.mgf", package = "fioRa")
  tmp <- fioRa:::read_fiora(fl = fl, fmt = "df")
  # return value is a data.frame
  testthat::expect_true(is.data.frame(tmp))
  # column "spec" is present
  testthat::expect_true("spec" %in% colnames(tmp))
  # column "spec" contains mz:int pairs
  #testthat::expect_true(all(tmp[,"spec"]))
})
