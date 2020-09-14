#' Sensor Data Preprocessing for METALP Data
#'
#' Run this addin to open a window to run some preprocessing tasks for the
#' METALP Sensor data.
#'
#' @export
metalpDataPreprocessing <- function() {
  ui <- miniPage()

  server <- function(input, output, session) {

  }

  viewer <- dialogViewer()
  runGadget(ui, server, viewer = viewer)
}
