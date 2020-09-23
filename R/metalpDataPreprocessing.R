#' Sensor Data Preprocessing for METALP Data
#'
#' Run this addin to open a window to run some preprocessing tasks for the
#' METALP Sensor data.
#'
#' @export
metalpDataPreprocessing <- function() {
  # Add the package www folder as resource for assets
  addResourcePath('metalpDP', system.file('www', package = "metalpDataPreprocessing"))

  ## Build UI ######################################################################
  ui <- miniPage(
    # Add CSS and JavaScript link tags to head
    tags$head(
      tags$link(href = 'metalpDP/main.css', rel = 'stylesheet', type = 'text/css'),
      tags$script(src = 'metalpDP/main.js')
    ),
    # Add the busy spinner (hidden until called)
    use_busy_spinner(spin = 'looping-rhombuses', color = '#112446' ,position = 'top-left', margins = c(15, 10)),
    sensorRawDataProcessingUI('rawData')
  )


  ## Server function ##############################################################

  server <- function(input, output, session) {
    # Create the possible roots for the folder browsing window
    roots <- c('root' = '/', 'home' = '~', 'working directory' = '.')

    callModule(sensorRawDataProcessing, 'rawData', roots)
  }

  viewer <- dialogViewer('METALP Data Preprocessing', width = 800, height = 800)
  # viewer <- browserViewer()
  runGadget(ui, server, viewer = viewer)
}
