testthat::test_that("selectInputWithButtons works", {

  choices_r <- shiny::reactiveVal(c("A", "B", "C"))

  shiny::testServer(
    app = selectInputWithButtonsServer,
    args = list(choices = choices_r),
    expr = {

      # Initially NULL
      testthat::expect_null(selected())

      # choices() triggers observeEvent to update SelectInput
      session$flushReact()
      session$setInputs(input = "A")
      testthat::expect_equal(input$input, "A")

      # Simulate selection of "B"
      session$setInputs(input = "B")
      testthat::expect_equal(selected(), "B")

      # Simulate "up"
      session$setInputs(up = 1)
      session$setInputs(input = "A")
      testthat::expect_equal(input$input, "A")
      testthat::expect_equal(selected(), "A")

      # Simulate "down"
      session$setInputs(down = 1)
      session$setInputs(input = "B")
      testthat::expect_equal(input$input, "B")
      testthat::expect_equal(selected(), "B")

      # change choices
      choices_r(c("X", "Y"))
      session$flushReact()
      session$setInputs(input = "X")
      testthat::expect_equal(input$input, "X")
    }
  )
})
