testthat::test_that("fioRa `find_fiora_predict_paths` returns valid installation details", {
  testthat::skip_on_cran()
  tmp <- suppressMessages(fioRa:::find_fiora_predict_paths())
  testthat::expect_length(tmp, 3)
  testthat::expect_true(all(c("os", "python", "script") %in% names(tmp)))
  testthat::expect_true(file.exists(tmp[["python"]]))
  testthat::expect_true(file.exists(tmp[["script"]]))
})

