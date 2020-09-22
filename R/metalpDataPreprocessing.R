#' Sensor Data Preprocessing for METALP Data
#'
#' Run this addin to open a window to run some preprocessing tasks for the
#' METALP Sensor data.
#'
#' @export
metalpDataPreprocessing <- function() {
  addResourcePath('metalpDP', system.file('www', package = "metalpDataPreprocessing"))
  ui <- miniPage(
    tags$head(
      tags$link(href = 'metalpDP/main.css', rel = 'stylesheet', type = 'text/css'),
      tags$script(src = 'metalpDP/main.js')
    ),
    gadgetTitleBar('Sensors raw data processing', left = NULL, right = miniTitleBarButton('run', 'Run',primary = TRUE)),
    div(
      class = 'content-wrapper grid-2',
      div(
        selectizeInput(
          inputId =  'sites',
          label = span('Stations', actionButton('allSites', 'All'), actionButton('clearSites', 'Clear')),
          choices = c('VAD', 'VAU', 'VEL', 'FED', 'FEU', 'PEU', 'AND', 'ANU', 'RIC', 'VID', 'VIM', 'VIU'),
          multiple = TRUE,
          options = list(
            'placeholder' = 'Select some stations...',
            'plugins' = list('remove_button')
          )
        ),
        selectizeInput(
          inputId =  'parameters',
          label = span('Parameters', actionButton('allParameters', 'All'), actionButton('clearParameters', 'Clear')),
          choices = c('BP', 'CO2atm', 'TURB', 'DO', 'CDOM', 'COND', 'PAR1', 'PAR2', 'DEPTH', 'pCO2'),
          multiple = TRUE,
          options = list(
            'placeholder' = 'Select some parameters...',
            'plugins' = list('remove_button')
          )
        )
      ),
      div(
        class = 'left-btns',
        div(
          class = 'input-dir',
          shinyDirButton('inputDir', 'Input Directory', 'Choose input directory'),
          textOutput('selectedInputDir', container = p),
        ),
        div(
          class = 'input-dir',
          shinyDirButton('outputDir', 'Output Directory', 'Choose output directory'),
          textOutput('selectedOutputDir', container = p),
        )
      ),
      pre(id = 'console-raw', class = 'console')
    )
  )

  server <- function(input, output, session) {
    roots <- c('root' = '/', 'home' = '~', 'working directory' = '.')


    ## Directories slection #######################################################

    shinyDirChoose(input, 'inputDir', roots = roots, defaultRoot = 'working directory')
    shinyDirChoose(input, 'outputDir', roots = roots, defaultRoot = 'working directory')

    output$selectedInputDir <- renderText(parseDirPath(roots, input$inputDir))
    output$selectedOutputDir <- renderText(parseDirPath(roots, input$outputDir))


    ## Stations and Parameters selection ##########################################

    observeEvent(input$allSites, ignoreInit = TRUE, {
      updateSelectizeInput(session, 'sites', selected = c('VAD', 'VAU', 'VEL', 'FED', 'FEU', 'PEU', 'AND', 'ANU', 'RIC', 'VID', 'VIM', 'VIU'))
    })

    observeEvent(input$allParameters, ignoreInit = TRUE, {
      updateSelectizeInput(session, 'parameters', selected = c('PAR1', 'PAR2', 'BP', 'CO2atm', 'COND', 'DO', 'CDOM', 'TURB', 'pCO2', 'DEPTH'))
    })

    observeEvent(input$clearSites, ignoreInit = TRUE, {
      updateSelectizeInput(session, 'sites', selected = '')
    })

    observeEvent(input$clearParameters, ignoreInit = TRUE, {
      updateSelectizeInput(session, 'parameters', selected = '')
    })



    ## Script running #############################################################

    observeEvent(input$run, ignoreInit = TRUE, {
      sendUpdateToConcole(id = 'console-raw', action = 'clear')

      withConsoleRedirect('console-raw', {
        combineSensorsDataPerSite(
          inputDir = parseDirPath(roots, input$inputDir),
          outputDir = parseDirPath(roots, input$outputDir),
          sites = input$sites,
          parameters = input$parameters
        )
      })
    })
  }

  viewer <- dialogViewer('METALP Data Preprocessing', width = 800, height = 800)
  # viewer <- browserViewer()
  runGadget(ui, server, viewer = viewer)
}
