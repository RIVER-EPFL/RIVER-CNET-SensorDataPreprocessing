#' Sensor Data Preprocessing for METALP Data
#'
#' Run this addin to open a window to run some preprocessing tasks for the
#' METALP Sensor data.
#'
#' @export
metalpDataPreprocessing <- function() {
  ui <- miniPage(
    includeCSS(system.file("www/main.css", package = "metalpDataPreprocessing")),
    gadgetTitleBar('Sensors raw data processing', left = miniTitleBarCancelButton(), right = NULL),
    div(
      class = 'content-wrapper',
      fillCol(
        selectizeInput(
          inputId =  'sites',
          label = 'Stations',
          choices = c('VAD', 'VAU', 'VEL', 'FED', 'FEU', 'PEU', 'AND', 'ANU', 'RIC', 'VID', 'VIM', 'VIU'),
          multiple = TRUE,
          options = list(
            'placeholder' = 'Select some stations...',
            'plugins' = list('remove_button')
          )
        ),
        selectizeInput(
          inputId =  'sites',
          label = 'Stations',
          choices = c('BP', 'CO2atm', 'TURB', 'DO', 'CDOM', 'COND', 'PAR1', 'PAR2', 'DEPTH', 'pCO2'),
          multiple = TRUE,
          options = list(
            'placeholder' = 'Select some stations...',
            'plugins' = list('remove_button')
          )
        )
      )
    )
  )

  server <- function(input, output, session) {

  }

  viewer <- dialogViewer('METALP Data Preprocessing')
  runGadget(ui, server, viewer = viewer)
}
