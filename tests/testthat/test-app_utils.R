testthat::test_that(
  desc = "next_choice funktioniert korrekt",
  code = {
    choices <- c("A", "B", "C")

    expect_equal(next_choice("B", choices, "up"), "A")
    expect_equal(next_choice("B", choices, "down"), "C")

    # edge cases
    expect_equal(next_choice("A", choices, "up"), "A")
    expect_equal(next_choice("C", choices, "down"), "C")

    # illegal case
    expect_equal(next_choice("X", choices, "up"), "X")
})
