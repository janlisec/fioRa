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
#'       fioRa:::page_fioRa_ui(id = "test")
#'     ),
#'     server = function(input, output, session) {
#'       fioRa:::page_fioRa_server(id = "test")
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
        bslib::accordion(id = ns("acc"), multiple = FALSE, open = "[Textbox] Copy/paste fioRa input",
          bslib::accordion_panel(
            "[File] Load fioRa input from csv-file",
            #shiny::fileInput(inputId = ns("file_input"), label = "Select csv input file (or enter compound info manually below)"),
            shiny::fileInput(inputId = ns("file_input"), label = NULL),
          ),
          bslib::accordion_panel(
            "[Form] Create fioRa input using form (single compound)",
            #shiny::textInput(inputId = ns("frm_name"), label = "Name", value = "Example_0"),
            shiny::textInput(inputId = ns("frm_smiles"), label = "SMILES", value = "CC1=CC(=O)OC2=CC(OS(O)(=O)=O)=CC=C12"),
            bslib::layout_columns(
              shiny::selectInput(inputId = ns("frm_ptype"), label = "Precursor_type", choices = c("[M+H]+","[M-H]-"), selected = "[M-H]-"),
              shiny::numericInput(inputId = ns("frm_ce"), label = "CE", value = 17, min = 1, max = 100, step = 1),
              shinyjs::disabled(shiny::selectInput(inputId = ns("frm_itype"), label = "Instrument_type", choices = c("HCD"))),
            ),
            shiny::actionButton(inputId = ns("frm_btn"), label = "Transfer to [Textbox]")
          ),
          bslib::accordion_panel(
            "[Textbox] Copy/paste fioRa input",
            #shiny::textAreaInput(inputId = ns("text_input"), label = "Enter infos for up to 10 compounds (one per row) in depicted format:", value = "Name,SMILES,Precursor_type,CE,Instrument_type\n", rows = 12)
            shiny::textAreaInput(inputId = ns("text_input"), label = NULL, value = "Name,SMILES,Precursor_type,CE,Instrument_type\n", rows = 12)
          )
        ),
        shiny::actionButton(inputId = ns("start_button"), "Process [Textbox] input")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "d-flex justify-content-between",
          #shiny::strong(shiny::actionLink(inputId = ns("fig1_link"), label = "Processing output")),
          shiny::div(style = "height: 72px; font-size: 1.25rem; padding-top: 38px;", "fioRa output"),
          shinyjs::hidden(shiny::div(
            id = ns("usr_opt"),
            shiny::div(style = "float: left; margin-left: 15px;", shiny::checkboxInput(inputId = ns("show_neutral_losses"), label = "show_neutral_losses", value = TRUE)),
            shiny::div(style = "float: left; margin-left: 15px;", shiny::selectInput(inputId = ns("name"), label = NULL, choices = "")),
            #shiny::div(style = "float: left; margin-left: 15px;", shiny::sliderInput(inputId = ns("digits"), label = NULL, min = -1, max = 4, value = 3, step = 1)),
            shiny::div(style = "float: left; margin-left: 15px;", shiny::downloadButton(outputId = ns("btn_download_msp"), label = ""))
          ))
        ),
        bslib::layout_sidebar(
          padding = 0,
          sidebar = bslib::sidebar(
            id = ns("sidebar_spec"),
            position = "right", open = FALSE, width = 320,
            shiny::tableOutput(outputId = ns("tab"))
          ),
          style = "resize:vertical;",
          #bslib::card_body(
            shiny::plotOutput(
              outputId = ns("spec"),
              dblclick = ns("spec_dblclick"),
              brush = brushOpts(id = ns("spec_brush"), resetOnNew = TRUE)
            )
          #)
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
    test_data <- c("Name,SMILES,Precursor_type,CE,Instrument_type", "Example_0,CC1=CC(=O)OC2=CC(OS(O)(=O)=O)=CC=C12,[M-H]-,17,HCD",
                   "Example_1,CC12CCC3C(CCC4=CC(OS(O)(=O)=O)=CC=C34)C1CCC2=O,[M-H]-,24,HCD",
                   "Example_2,ClC1=CC=C(NC(=O)NC2=CC=C(Cl)C(Cl)=C2)C=C1,[M-H]-,21,HCD",
                   "Example_3,Oc1c(Cl)cc(Cl)cc1C(=O)Nc1ccc(Cl)c(Cl)c1,[M-H]-,24,HCD",
                   "Example_4,NC(=O)\\C=C\\C1=CC=CC=C1,[M+H]+,10,HCD", "Example_5,ClC1=CC2=C(C=C1)N(C1CCN(CCCN3C(=O)NC4=C3C=CC=C4)CC1)C(=O)N2,[M+H]+,29,HCD",
                   "Example_6,C1CCC(CC1)NC1=NC2=CC=CC=C2S1,[M+H]+,16,HCD", "Example_7,CCCCC1=C(C)N=C(NCC)N=C1OS(=O)(=O)N(C)C,[M+H]+,22,HCD",
                   "Example_8,CC(C)(C)C1=CC=C(C=C1)C(=O)CCCN1CCC(CC1)OC(C1=CC=CC=C1)C1=CC=CC=C1,[M+H]+,32,HCD",
                   "Example_9,FC1=CC(OC2=CC=C(C=C2Cl)C(F)(F)F)=CC=C1NC(=O)NC(=O)C1=C(F)C=CC=C1F,[M+H]+,34,HCD",
                   ""
    )

    rv <- shiny::reactiveValues(
      "fiora_finished" = 0,
      "test_data" = test_data,
      "res" = NULL
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

    shiny::observeEvent(rv$fiora_finished, {
      if (!file.exists(temp_output_file)) {
        message("Could not create output file. Check logs.")
      } else {
        res <- read_fiora(fl = temp_output_file)
        shiny::updateSelectInput(inputId = "name", choices = names(res))
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
      req(input$name, rv$res)
      message("generating plot")
      plot_spec(s = rv$res[[input$name]][["spec"]], show_neutral_losses = input$show_neutral_losses, xlim = ranges$x, ylim = ranges$y)
    })

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
      req(input$name, rv$res)
      rv$res[[input$name]][["spec"]][,c("mz","int")]
    }, striped = TRUE, hover = TRUE, digits = 4)

    shiny::observeEvent(input$file_input, {
      tmp_data <- readLines(input$file_input$datapath)
      shiny::updateTextAreaInput(inputId = "text_input", value = paste(tmp_data, collapse="\n"))
      bslib::accordion_panel_open(id = "acc", values = "[Textbox] Copy/paste fioRa input")
    })

    shiny::observeEvent(input$frm_btn, {
      tmp_data <- c(
        "Name,SMILES,Precursor_type,CE,Instrument_type",
        paste("Name", input$frm_smiles, input$frm_ptype, input$frm_ce, input$frm_itype, sep=","),
        ""
      )
      shiny::updateTextAreaInput(inputId = "text_input", value = paste(tmp_data, collapse="\n"))
      bslib::accordion_panel_open(id = "acc", values = "[Textbox] Copy/paste fioRa input")
      #start_button_events()
    })

  })
}
