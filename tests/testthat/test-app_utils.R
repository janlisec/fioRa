testthat::test_that(
  desc = "next_choice works correctly",
  code = {
    choices <- c("A", "B", "C")
    # standard case
    testthat::expect_equal(fioRa:::next_choice("B", choices, "up"), "A")
    testthat::expect_equal(fioRa:::next_choice("B", choices, "down"), "C")
    # edge cases
    testthat::expect_equal(fioRa:::next_choice("A", choices, "up"), "A")
    testthat::expect_equal(fioRa:::next_choice("C", choices, "down"), "C")
    # illegal case
    testthat::expect_equal(fioRa:::next_choice("X", choices, "up"), "X")
  }
)

testthat::test_that(
  desc = "is_valid_path works correctly",
  code = {
    # return FALSE for no character vector or vector longer than 1 or empty character
    testthat::expect_false(fioRa:::is_valid_path(123))
    testthat::expect_false(fioRa:::is_valid_path(c("1","2")))
    testthat::expect_false(fioRa:::is_valid_path(""))
    # return TRUE for possible path
    testthat::expect_true(fioRa:::is_valid_path("test"))
  }
)

testthat::test_that(
  desc = "estimateSelectWidth works correctly",
  code = {
    long_string <- paste(LETTERS, collapse="")
    out_exp <- paste0(10+16+nchar(long_string)*8, "px")
    testthat::expect_equal(fioRa:::estimateSelectWidth(NULL), "120px")
    testthat::expect_equal(fioRa:::estimateSelectWidth(c("1","2")), "120px")
    testthat::expect_equal(fioRa:::estimateSelectWidth(c("1", long_string)), out_exp)
    testthat::expect_equal(fioRa:::estimateSelectWidth(c("1", long_string), max_width = 160), "160px")
  }
)

testthat::test_that(
  desc = "renderSMILES works correctly",
  code = {
    vdiffr::expect_doppelganger(
      title = "renderSMILES kekulise=FALSE",
      fig = function() {
        pdf(NULL)
        smiles <- "OS(=O)(=O)c1ccc(cc1)C(CC(=O)O)CC(=O)O"
        plot.new()
        plot.window(xlim=c(0,200), ylim=c(0,100))
        fioRa:::renderSMILES(smiles, kekulise=FALSE)
      }
    )
    vdiffr::expect_doppelganger(
      title = "renderSMILES kekulise=TRUE",
      fig = function() {
        pdf(NULL)
        smiles <- "OS(=O)(=O)c1ccc(cc1)C(CC(=O)O)CC(=O)O"
        plot.new()
        plot.window(xlim=c(0,200), ylim=c(0,100))
        fioRa:::renderSMILES(smiles, kekulise=TRUE)
      }
    )
  }
)

testthat::test_that(
  desc = "plotSpec works correctly",
  code = {
    fl <- system.file("extdata/annotated_output.mgf", package = "fioRa")
    tmp <- fioRa::read_fiora(fl = fl)
    s <- tmp[[1]][["spec"]]
    vdiffr::expect_doppelganger(
      title = "plot_spec Standard",
      fig = function() {
        pdf(NULL)
        fioRa::plot_spec(s = s)
      }
    )
  }
)

testthat::test_that(
  desc = "add_adduct works correctly",
  code = {
    # standard case
    testthat::expect_equal(fioRa:::add_adduct(fml = "CHO", ad = "[M+2H]+"), "C1H3O1")
    # no valid adduct specified (error)
    testthat::expect_error(testthat::expect_message(fioRa:::add_adduct(fml = "CHO", ad = "[quatsch]")))
  }
)

testthat::test_that(
  desc = "square_subplot_coord returns 4 numeric values",
  code = {
    grDevices::png(filename = tempfile(), width = 800, height = 600)
    on.exit(grDevices::dev.off())

    graphics::plot(1:10, 1:10)
    coords <- fioRa:::square_subplot_coord(5, 5, w = 0.2)

    testthat::expect_type(coords, "double")
    testthat::expect_length(coords, 4)
  }
)

testthat::test_that(
  desc = "square_subplot_coord keeps center at x for inner points",
  code = {
    grDevices::png(filename = tempfile(), width = 800, height = 600)
    on.exit(grDevices::dev.off())

    graphics::plot(0:10, 0:10)
    coords <- fioRa:::square_subplot_coord(5, 5, w = 0.2)

    center_x <- (coords[1] + coords[2]) / 2
    testthat::expect_equal(center_x, 5, tolerance = 1e-6)
  }
)

testthat::test_that(
  desc = "verify_suggested works correctly",
  code = {
    # returning error for packages not present
    testthat::expect_error(fioRa:::verify_suggested("this_is_not_a_package"))
  }
)

testthat::test_that("plot_spec runs without error", {
  s <- data.frame(
    mz = c(100, 150, 200),
    int = c(1, 5, 3),
    formula = c("H2O", "C6H6", "CO2"),
    adduct = c("[M+H]+", "[M+H]+", "[M+H]+"),
    SMILES = c("O", "c1ccccc1", "O=C=O")
  )

  res <- fioRa::plot_spec(s, show_neutral_losses = FALSE, show_smiles = FALSE)
  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(nrow(res), 3)
})
