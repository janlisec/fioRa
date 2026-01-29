#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  # htis is the padding definition for all panels to respect navbar and footer
  navbar_padding <- "56px"
  footer_padding <- "48px"
  nps <- paste0("padding-top: ", navbar_padding, "; padding-bottom: ", footer_padding)

  tagList(
    # keep this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      id = "navbarpage",
      title = list(
        shiny::img(src = "www/bam_logo_200px_transparent.png", height = "40px", position = "absolute", margin = "auto", alt = "BAM Logo"),
        shiny::strong("BAM", style = "color: rgb(210,0,30);"),
        shiny::em(get_golem_config("app_name"), style = "color: rgb(0,175,240);")
      ),
      selected = "Predict",
      navbar_options = list(
        bg = "black",
        position = "fixed-top"
      ),
      footer = shiny::div(
        style = "padding-left: var(--bslib-spacer, 1rem); font-family: var(--bs-font-monospace); position: fixed; bottom: 0; background-color: black; color: white; width: 100%",
        shiny::HTML(
          get_golem_config("app_name"), "|",
          get_golem_config("app_version"), "|",
          get_golem_config("app_date"), "|",
          '<a href="mailto:jan.lisec@bam.de">jan.lisec@bam.de</a>',
          ifelse(get_golem_config("bam_server"), '| <a href="https://www.bam.de/Navigation/EN/Services/Privacy-Policy/privacy-policy.html" target="_blank" rel="noopener noreferrer">BAM Privacy Policy</a>', "")
        )
      ),
      bslib::nav_panel(
        id = "panel_fioRa",
        title = "Predict",
        icon = shiny::icon("angle-right"),
        shiny::div(style = nps, page_fioRa_ui("fioRa"))
      ),
      bslib::nav_panel(
        id = "panel_help",
        title = "Help",
        icon = shiny::icon("angle-right"),
        shiny::div(
          style = paste0(nps, "; width: 780px"),
          shiny::tagList(
            shiny::p(
              'Please go to the',
              shiny::a(href="https://github.com/janlisec/fioRa", target="_blank", rel="noopener noreferrer", "GitHub page"),
              'to read documentation on the', shiny::strong("fioRa"), 'package.'
            ),
            shiny::p(
              'Use the', shiny::strong("fioRa input"), 'section from the foldable menu on the left to specify compounds, for which you would like',
              shiny::strong("fioRa"), 'to predict an MS2 spectrum for. Your options to provide SMILES codes are (i) by uploading a csv file,',
              '(ii) by entering a SMILES in a simple form or (iii) by pasting directly to a textbox.'
            ),
            shiny::p(
              'Processing is always started using the respective button below the textbox. Processing is limited to 10 compounds.',
              'o process larger numbers, please install ', shiny::strong("fioRa"), ' locally and use function run_script().'
            ),
            shiny::p(
              'After the processing is finished the ', shiny::strong("output"), ' section is propagated with the results.',
              'You may inspect the predicted spectra for each compound or download the result file in msp-format.'
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path("www", app_sys("app/www"))
  tags$head(
    golem::favicon(ico = "BAMLogo"),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = get_golem_config("app_name")
    ),
    waiter::use_waiter(),
    shinyjs::useShinyjs()
  )
}
