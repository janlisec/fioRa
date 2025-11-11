testthat::test_that("selectInputWithButtons works", {

  choices_r <- shiny::reactiveVal(c("A", "B", "C"))

  testServer(selectInputWithButtonsServer, args = list(choices = choices_r), {

    # Initially NULL
    testthat::expect_null(selected())

    # choices() triggers observeEvent to update SelectInput
    session$flushReact()
    session$setInputs(input = "A")
    expect_equal(input$input, "A")

    # Simulate selection of "B"
    session$setInputs(input = "B")
    expect_equal(selected(), "B")

    # Simulate "up"
    session$setInputs(up = 1)
    session$setInputs(input = "A")
    expect_equal(input$input, "A")
    expect_equal(selected(), "A")

    # Simulate "down"
    session$setInputs(down = 1)
    session$setInputs(input = "B")
    expect_equal(input$input, "B")
    expect_equal(selected(), "B")

    # change choices
    choices_r(c("X", "Y"))
    session$flushReact()
    session$setInputs(input = "X")
    expect_equal(input$input, "X")
  })
})
