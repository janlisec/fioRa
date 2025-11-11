#' selectInputWithButtons UI Function
#'
#' @description A shiny Module to integrate an inputSelect which is extended by
#'     two buttons.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param label label of inputSelect.
#' @examples
#' if (interactive()) {
#'   shiny::shinyApp(
#'     ui = bslib::page_fluid(
#'       shinyjs::useShinyjs(),
#'       fioRa:::selectInputWithButtonsUI(id = "test")
#'     ),
#'     server = function(input, output, session) {
#'       #choices <- shiny::reactiveVal(c("A","123","ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
#'       choices <- shiny::reactiveVal(c("A","123","Z"))
#'       x <- fioRa:::selectInputWithButtonsServer(id = "test", choices = choices)
#'       shiny::observeEvent(x(), { message(x()) }, ignoreNULL = TRUE)
#'     }
#'   )
#' }
#' @keywords internal
#' @noRd

selectInputWithButtonsUI <- function(id, label = NULL) {
  ns <- NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
        .select-nav {
          display: flex;
          align-items: flex-start;
          gap: 2px;
        }
        .arrow-column {
          display: flex;
          flex-direction: column;
          gap: 2px;
        }
        .arrow-btn {
          height: 16px;
          width: 16px;
          padding: 0;
          font-size: 12px;
          line-height: 12px;
        }
        .select-nav.with-label .arrow-column {
          padding-top: 24px; /* Abstand f\u00fcr Labelh\u00f6he */
        }
      "))
    ),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('setSelectWidth', function(message) {
        var el = document.getElementById(message.id);
        if (el) {
          el.style.width = message.width;
        }
      });
    ")),
    shiny::div(id = ns("input_div"), class = paste("select-nav", if (!is.null(label)) "with-label"),
        shiny::selectInput(ns("input"), label = label, choices = NULL),
        shiny::div(
          class = "arrow-column",
          shiny::actionButton(ns("up"), label = "\u25B2", class = "arrow-btn"),
          shiny::actionButton(ns("down"), label = "\u25BC", class = "arrow-btn")
        )
    )
  )
}

#' selectInputWithButtons Server Function
#' @param choices Reactive value of choices for selectInput.
#' @param default default selected value from choices.
#' @keywords internal
#' @noRd

selectInputWithButtonsServer <- function(id, choices, default = NULL) {

  stopifnot(is.reactive(choices))

  moduleServer(id, function(input, output, session) {

    selected <- shiny::reactiveVal(default)
    idx <- shiny::reactiveVal()

    shiny::observeEvent(choices(), {
      shiny::updateSelectInput(session, inputId = "input", choices = choices(), selected = choices()[1])
      session$sendCustomMessage("setSelectWidth", list(id = session$ns("input_div"), width = estimateSelectWidth(choices())))
    })

    shiny::observeEvent(input$input, {
      req(choices())
      index <- match(input$input, choices())
      if (!is.na(index)) {
        shinyjs::toggleState("up", condition = index > 1)
        shinyjs::toggleState("down", condition = index < length(choices()))
        selected(input$input)
      }
    }, ignoreNULL = TRUE)

    shiny::observeEvent(input$up, {
      updateSelectInput(session, "input", selected = next_choice(input$input, choices(), "up"))
    })

    shiny::observeEvent(input$down, {
      updateSelectInput(session, "input", selected = next_choice(input$input, choices(), "down"))
    })

    return(selected)
  })
}
