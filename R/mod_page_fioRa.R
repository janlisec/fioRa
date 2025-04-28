#' page_fioRa UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
page_fioRa_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_sidebar(
      padding = 0,
      sidebar = bslib::sidebar(
        id = ns("sidebar_input"),
        title = "fioRa Input", position = "left", open = "open", width = 720,
        shiny::fileInput(inputId = ns("file_input"), label = "Select csv input file (or enter compound info manually below)"),
        shiny::textAreaInput(inputId = ns("text_input"), label = "Enter infos for up to 10 compounds (one per row) in depicted format:", value = "Name,SMILES,Precursor_type,CE,Instrument_type\n", rows = 12),
        shiny::actionButton(inputId = ns("start_button"), "Process input")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          class = "d-flex justify-content-between",
          #shiny::strong(shiny::actionLink(inputId = ns("fig1_link"), label = "Processing output")),
          shiny::div(style = "height: 72px; font-size: 1.25rem; padding-top: 38px;", "fioRa output"),
          shinyjs::hidden(shiny::div(
            id = ns("usr_opt"),
            shiny::div(style = "float: left; margin-left: 15px;", shiny::selectInput(inputId = ns("name"), label = NULL, choices = "")),
            shiny::div(style = "float: left; margin-left: 15px;", shiny::sliderInput(inputId = ns("digits"), label = NULL, min = -1, max = 4, value = 3, step = 1)),
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
#'
#' @noRd
page_fioRa_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # # ensure that reticulate is set up and that fiora is installed
    # msg <- reticulate::py_require("git+https://github.com/BAMeScience/fiora.git")

    waiter::waiter_show(html = tagList(waiter::spin_fading_circles(), "fioRa is still loading..."))

    # get location of fiora-predict script
    fiora_script <- list.files(path=reticulate::py_config()$virtualenv, pattern="^fiora-predict$", recursive = TRUE, full.names = TRUE)
    if (!file.exists(fiora_script)) message("Could not detect script 'fiora-predict'") else { message("fiora_script: ", fiora_script) }

    # write test data to input file
    temp_input_file <- tempfile(fileext = ".csv")
    temp_output_file <- gsub("csv$", "mgf", temp_input_file)
    test_data <- c("Name,SMILES,Precursor_type,CE,Instrument_type", "Example_0,CC1=CC(=O)OC2=CC(OS(O)(=O)=O)=CC=C12,[M-H]-,17,HCD",
                   #"Example_1,CC12CCC3C(CCC4=CC(OS(O)(=O)=O)=CC=C34)C1CCC2=O,[M-H]-,24,HCD",
                   "Example_2,ClC1=CC=C(NC(=O)NC2=CC=C(Cl)C(Cl)=C2)C=C1,[M-H]-,21,HCD",
                   # "Example_3,Oc1c(Cl)cc(Cl)cc1C(=O)Nc1ccc(Cl)c(Cl)c1,[M-H]-,24,HCD",
                   # "Example_4,NC(=O)\\C=C\\C1=CC=CC=C1,[M+H]+,10,HCD", "Example_5,ClC1=CC2=C(C=C1)N(C1CCN(CCCN3C(=O)NC4=C3C=CC=C4)CC1)C(=O)N2,[M+H]+,29,HCD",
                   # "Example_6,C1CCC(CC1)NC1=NC2=CC=CC=C2S1,[M+H]+,16,HCD", "Example_7,CCCCC1=C(C)N=C(NCC)N=C1OS(=O)(=O)N(C)C,[M+H]+,22,HCD",
                   # "Example_8,CC(C)(C)C1=CC=C(C=C1)C(=O)CCCN1CCC(CC1)OC(C1=CC=CC=C1)C1=CC=CC=C1,[M+H]+,32,HCD",
                   # "Example_9,FC1=CC(OC2=CC=C(C=C2Cl)C(F)(F)F)=CC=C1NC(=O)NC(=O)C1=C(F)C=CC=C1F,[M+H]+,34,HCD",
                   ""
    )

    rv <- shiny::reactiveValues(
      "fiora_finished" = 0,
      "test_data" = test_data,
      "res" = NULL
    )

    shiny::observeEvent(rv$test_data, {
      shiny::updateTextAreaInput(inputId = "text_input", value = paste(test_data, collapse="\n"))
      waiter::waiter_hide()
    })

    shiny::observeEvent(input$start_button, {
      bslib::toggle_sidebar(id = "sidebar_input", open = FALSE, session = session)
      bslib::toggle_sidebar(id = "sidebar_spec", open = TRUE, session = session)
      waiter::waiter_show(id = ns("sidebar_input"), html = tagList(waiter::spin_fading_circles(), "fioRa is processing your input..."))
        # write current text input to file
        cat(input$text_input, file = temp_input_file, append = FALSE)

        # establish system command and args and run script
        command <- reticulate::py_config()$python
        args <- c(fiora_script, paste0('-i \"', temp_input_file, '\"'), paste0('-o \"', temp_output_file, '\"'))
        msg <- system2(command = command, args = args)
        if (msg==0) rv$fiora_finished <- rv$fiora_finished+1
      waiter::waiter_hide()
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
      filename = function() { "fioRa_result.txt" },
      content = function(file) { file.copy(from = temp_output_file, to = file, overwrite = TRUE) }
    )

    # Single zoomable plot (on left)
    ranges <- reactiveValues(x = NULL, y = NULL)

    output$spec <- shiny::renderPlot({
      req(input$name, rv$res)
      s <- rv$res[[input$name]][["spec"]]
      x = s[,"mz"]
      y = s[,"int"]
      plot(x = x, y = y, type="h", xlab="m/z", ylab="Int (relative)", cex.axis=1.25, cex.lab=1.25, xlim = ranges$x, ylim = ranges$y)
      n <- input$digits
      if (n>=0) {
        flt <- y > 0.05*max(y, na.rm=T) & y < 0.9*max(y, na.rm=T)
        #text(x = x[flt], y = y[flt], labels=round(x[flt],n), pos=3, col=4, srt=90)
        text(x = x[flt], y = y[flt], labels=round(x[flt],n), pos=3, col=4)
        flt <- y > 0.9*max(y, na.rm=T)
        #text(x = x[flt], y = y[flt], labels=round(x[flt],n), pos=1, col=4, srt=90)
        text(x = x[flt], y = y[flt], labels=round(x[flt],n), pos=1, col=4)
      }
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
    })

    output$tab <- shiny::renderTable({
      req(input$name, rv$res)
      rv$res[[input$name]][["spec"]]
    }, striped = TRUE, hover = TRUE, digits = 4)

    shiny::observeEvent(input$file_input, {
      tmp_data <- readLines(input$file_input$datapath)
      shiny::updateTextAreaInput(inputId = "text_input", value = paste(tmp_data, collapse="\n"))
    })

  })
}

## To be copied in the UI
# mod_page_fioRa_ui("page_fioRa_1")

## To be copied in the server
# mod_page_fioRa_server("page_fioRa_1")
