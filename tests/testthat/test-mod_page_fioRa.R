shiny::testServer(
  fioRa:::page_fioRa_server,
  # Add here your module params
  args = list(), {
    ns <- session$ns
    testthat::expect_true(
      inherits(ns, "function")
    )
    testthat::expect_true(
      grepl(id, ns(""))
    )
    testthat::expect_true(
      grepl("test", ns("test"))
    )
    # Here are some examples of tests you can
    # run on your module
    # - Testing the setting of inputs
    # session$setInputs(x = 1)
    # expect_true(input$x == 1)
    # - If ever your input updates a reactiveValues
    # - Note that this reactiveValues must be passed
    # - to the testServer function via args = list()
    # expect_true(r$x == 1)
    # - Testing output
    # expect_true(inherits(output$tbl$html, "html"))
  }
)

testthat::test_that("module ui works", {
  ui <- fioRa:::page_fioRa_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(fioRa:::page_fioRa_ui)
  for (i in c("id")){
    testthat::expect_true(i %in% names(fmls))
  }
})

