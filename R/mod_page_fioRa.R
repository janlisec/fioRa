#' page_fioRa UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @examples
#' if (interactive()) {
#'   shiny::shinyApp(
#'     ui = bslib::page_fluid(
#'       shinyjs::useShinyjs(),
#'       waiter::useWaiter(),
#'       fioRa:::page_fioRa_ui(id = "page_fioRa")
#'     ),
#'     server = function(input, output, session) {
#'       fioRa:::page_fioRa_server(id = "page_fioRa")
#'     }
#'   )
#' }
#' @keywords internal
#' @noRd
page_fioRa_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_sidebar(
      padding = 0,
      sidebar = bslib::sidebar(
        id = ns("sidebar_input"),
        #title = "fioRa Input",
        position = "left", open = "open", width = 720,
        shiny::div(
        bslib::card_header(shiny::div(style = "font-size: 1.25rem; font-weight: 600; padding-top: 8px; padding-bottom: 2px;", "fioRa input")),
        bslib::accordion(id = ns("acc"), multiple = FALSE, open = "[Textbox] copy/paste fioRa input",
          bslib::accordion_panel(
            "[File-Input] Load fioRa input from csv-file",
            shiny::fileInput(inputId = ns("file_input"), label = NULL),
          ),
          bslib::accordion_panel(
            "[Form-Input] Create fioRa input using form (single compound)",
            shiny::textInput(inputId = ns("frm_smiles"), label = "SMILES", value = "CC1=CC(=O)OC2=CC(OS(O)(=O)=O)=CC=C12"),
            bslib::layout_columns(
              shiny::selectInput(inputId = ns("frm_ptype"), label = "Precursor_type", choices = c("[M+H]+","[M-H]-"), selected = "[M-H]-"),
              shiny::numericInput(inputId = ns("frm_ce"), label = "CE", value = 17, min = 1, max = 100, step = 1),
              shinyjs::disabled(shiny::selectInput(inputId = ns("frm_itype"), label = "Instrument_type", choices = c("HCD"))),
            ),
            shiny::actionButton(inputId = ns("frm_btn"), label = "Transfer to [Textbox]")
          ),
          bslib::accordion_panel(
            "[Textbox] copy/paste fioRa input",
            #shiny::textAreaInput(inputId = ns("text_input"), label = "Enter infos for up to 10 compounds (one per row) in depicted format:", value = "Name,SMILES,Precursor_type,CE,Instrument_type\n", rows = 12)
            shiny::textAreaInput(inputId = ns("text_input"), label = NULL, value = "Name,SMILES,Precursor_type,CE,Instrument_type\n", rows = 18),
            bslib::layout_columns(
              col_widths = c(6, 6),
              shiny::actionButton(inputId = ns("start_button"), "Process [Textbox] input"),
              shiny::radioButtons(inputId = ns("par_scale"), label = "Scale ion intensities to max of", choices = list("as is"=0, "1"=1, "100"=100, "999"=999), inline=TRUE),
            )
          )
        ))
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "d-flex justify-content-between",
          shiny::div(style = "height: 72px; font-size: 1.25rem; padding-top: 48px;", "fioRa output"),
          shinyjs::hidden(shiny::div(
            id = ns("usr_opt"),
            shiny::div(
              style = "float: left; margin-left: 15px; width: 260px;",
              shiny::helpText(
                "To zoom in: select a region with click and drag and double click to confirm.",
                "To zoom out: double click within plot but with no region selected."
              )
            ),
            shiny::div(
              style = "float: left; margin-left: 15px; width: 260px;",
              shiny::sliderInput(
                inputId = ns("masslab"), label = "Annotate above relative intensity of", min = 0, max = 0.25, value = 0.02, step = 0.01
              )
            ),
            shiny::div(
              style = "float: left; margin-left: 15px; width: 180px;",
              shiny::checkboxGroupInput(
                inputId = ns("plot_opt"), label = "overlay",
                choices = list("neutral losses"="losses", "chemical structure"="smiles"),
                selected = c("losses", "smiles")
              )
            ),
            shiny::div(
              style = "float: left; margin-left: 5px; height: 38px;",
              selectInputWithButtonsUI(id = ns("current_name"))
            ),
            shiny::div(
              style = "float: left; margin-left: 35px; margin-right: 24px; width: 60px; height: 38px;",
              shiny::downloadButton(outputId = ns("btn_download_msp"), label = "", width = 40)
            )
          ))
        ),
        bslib::layout_sidebar(
          padding = 0,
          sidebar = bslib::sidebar(
            id = ns("sidebar_spec"),
            position = "right", open = FALSE, width = 480,
            shiny::tableOutput(outputId = ns("tab"))
          ),
          style = "resize:vertical;",
          shiny::plotOutput(
            outputId = ns("spec"),
            height = "800px",
            dblclick = ns("spec_dblclick"),
            brush = brushOpts(id = ns("spec_brush"), resetOnNew = TRUE)
          )
        )
      )
    )
  )
}

#' page_fioRa Server Functions
#' @param fiora_script Python script fiora-predict.
#' @keywords internal
#' @noRd
page_fioRa_server <- function(id, fiora_script = "/home/shiny_test/miniforge3/envs/fiora/bin/fiora-predict"){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # write test data to input file
    temp_input_file <- tempfile(fileext = ".csv")
    temp_output_file <- gsub("csv$", "mgf", temp_input_file)
    test_data <- fioRa::test_data

    rv <- shiny::reactiveValues(
      "fiora_finished" = 0,
      "test_data" = test_data,
      "res" = NULL,
      "choices" = NULL,
      "name" = NULL
    )

    shiny::observeEvent(rv$test_data, {
      shiny::updateTextAreaInput(inputId = "text_input", value = paste(test_data, collapse="\n"))
    })

    start_button_events <- function() {
      bslib::toggle_sidebar(id = "sidebar_input", open = FALSE, session = session)
      bslib::toggle_sidebar(id = "sidebar_spec", open = TRUE, session = session)
      waiter::waiter_show(id = ns("sidebar_input"), html = tagList(waiter::spin_fading_circles(), "fioRa is processing your input..."))
      # write current text input to file
      text_input <- input$text_input
      n <- gregexpr("\n", text_input)[[1]]
      n_max <- 10
      if ((1+length(n))>n_max) {
        message("fioRa processing is limited to ", n_max, " compounds at max")
        text_input <- substr(text_input, 1, n[1+n_max])
      }
      cat(text_input, file = temp_input_file, append = FALSE)

      # establish system command and args and run script
      fioRa_pth <- find_fiora_predict_paths()
      args <- c(paste0('-i \"', temp_input_file, '\"'), paste0('-o \"', temp_output_file, '\"'), "--annotation")
      if (fioRa_pth$os == "Windows") {
        msg <- system2(command = fioRa_pth$python, args = c(fioRa_pth$script, args))
      } else {
        msg <- system2(command = fioRa_pth$script, args = args)
      }

      if (msg==0) rv$fiora_finished <- rv$fiora_finished+1
      waiter::waiter_hide()
    }

    shiny::observeEvent(input$start_button, {
      start_button_events()
    })

    name_choices <- shiny::reactiveVal()
    rv$name <- selectInputWithButtonsServer(id = "current_name", choices = name_choices)

    shiny::observeEvent(rv$fiora_finished, {
      if (!file.exists(temp_output_file)) {
        message("Could not create output file. Check logs.")
      } else {
        res <- read_fiora(fl = temp_output_file, fmt = "list", check = TRUE, scale = as.numeric(input$par_scale))
        rv$choices <- names(res)
        name_choices(names(res))
        shinyjs::show(id = "usr_opt")
        rv$res <- res
      }
    }, ignoreInit = TRUE)

    output$btn_download_msp <- shiny::downloadHandler(
      filename = function() { "fioRa_result.mgf" },
      content = function(file) { file.copy(from = temp_output_file, to = file, overwrite = TRUE) }
    )

    # Single zoomable plot (on left)
    ranges <- reactiveValues(x = NULL, y = NULL)

    output$spec <- shiny::renderPlot({
      shiny::validate(shiny::need(rv$res, "Spectrum plot will be shown after calculation is finished"))
      req(rv$name())
      plot_spec(s = rv$res[[rv$name()]][["spec"]], show_neutral_losses = "losses" %in% input$plot_opt, show_smiles = "smiles" %in% input$plot_opt, masslab = as.numeric(input$masslab), xlim = ranges$x, ylim = ranges$y)
    }, res = 72*1.5)

    # When a double-click happens, check if there's a brush on the plot.
    # If so, zoom to the brush bounds; if not, reset the zoom.
    observeEvent(input$spec_dblclick, {
      brush <- input$spec_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
      message("double click in plot to zoom. x = ", ranges$x, ", y = ", ranges$y)
    })

    output$tab <- shiny::renderTable({
      shiny::validate(shiny::need(rv$res, "Spectrum table will be shown after calculation is finished"))
      req(rv$name())
      rv$res[[rv$name()]][["spec"]][,c("mz","int","formula","adduct")]
      #rv$res[[rv$name()]][["spec"]][,c("mz","int")]
    }, striped = TRUE, hover = TRUE, digits = 4)

    shiny::observeEvent(input$file_input, {
      tmp_data <- readLines(input$file_input$datapath)
      shiny::updateTextAreaInput(inputId = "text_input", value = paste(tmp_data, collapse="\n"))
      bslib::accordion_panel_open(id = "acc", values = "[Textbox] copy/paste fioRa input")
    })

    shiny::observeEvent(input$frm_btn, {
      tmp_data <- c(
        "Name,SMILES,Precursor_type,CE,Instrument_type",
        paste("Name", input$frm_smiles, input$frm_ptype, input$frm_ce, input$frm_itype, sep=","),
        ""
      )
      shiny::updateTextAreaInput(inputId = "text_input", value = paste(tmp_data, collapse="\n"))
      bslib::accordion_panel_open(id = "acc", values = "[Textbox] copy/paste fioRa input")
    })

  })
}
