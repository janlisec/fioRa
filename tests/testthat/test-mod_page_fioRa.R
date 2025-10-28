shiny::testServer(
  fioRa:::page_fioRa_server,
  # Add here your module params
  args = list(id = "fioRa"), {
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

    #session$setInputs(`fioRa-start_button` = 1)
    #Sys.sleep(15)

    # Prüfe, ob der Plot-Output existiert
    #expect_true(!is.null(output$spec))


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

