testthat::test_that(
  desc = "app ui",
  code = {
    ui <- fioRa:::app_ui()
    golem::expect_shinytaglist(ui)
    # Check that formals have not been removed
    fmls <- formals(app_ui)
    for (i in c("request")) {
      testthat::expect_true(i %in% names(fmls))
    }
  }
)

testthat::test_that(
  desc = "app server",
  code = {
    server <- app_server
    testthat::expect_type(server, "closure")
    # Check that formals have not been removed
    fmls <- formals(app_server)
    for (i in c("input", "output", "session")) {
      testthat::expect_true(i %in% names(fmls))
    }
  }
)

testthat::test_that(
  desc = "app_sys works",
  code = {
    testthat::expect_true(
      app_sys("golem-config.yml") != ""
    )
  }
)

testthat::test_that(
  desc = "golem-config works",
  code = {
    config_file <- app_sys("golem-config.yml")
    skip_if(config_file == "")

    testthat::expect_true(
      get_golem_config(
        "app_prod",
        config = "production",
        file = config_file
      )
    )
    testthat::expect_false(
      get_golem_config(
        "app_prod",
        config = "dev",
        file = config_file
      )
    )
  }
)

# Configure this test to fit your need.
# testServer() function makes it possible to test code in server functions and modules, without needing to run the full Shiny application
shiny::testServer(
  app = fioRa:::app_server,
  expr = {

    # Set and test an input
    session$setInputs(x = 2)
    testthat::expect_equal(input$x, 2)

    # Example of tests you can do on the server:
    # - Checking reactiveValues
    # expect_equal(r$lg, 'EN')
    # - Checking output
    # expect_equal(output$txt, "Text")
  }
)

# Configure this test to fit your need
testthat::test_that(
  desc = "app launches",
  code = {
    golem::expect_running(sleep = 5)
  }
)
