## Module containing the Sensor Raw data processing

## UI function #####################################################################

sensorRawDataProcessingUI <- function(id) {
  # Create namespace
  ns <- NS(id)

  tagList(
    # Create a title bar with a run button
    gadgetTitleBar(
      'Sensors raw data processing',
      left = NULL,
      right = span(
        actionButton(ns('help'), icon('question-circle'), class = 'icon-btn'),
        miniTitleBarButton(ns('run'), 'Run', primary = TRUE)
      )
    ),
    # Create the content of the gadget as a grid
    div(
      class = 'content-wrapper grid-2',
      # Create the select inputs
      div(
        selectizeInput(
          inputId =  ns('sites'),
          label = span('Stations', actionButton(ns('allSites'), 'All'), actionButton(ns('clearSites'), 'Clear')),
          choices = c('VAD', 'VAU', 'VEL', 'FED', 'FEU', 'PEU', 'AND', 'ANU', 'RIC', 'VID', 'VIM', 'VIU'),
          multiple = TRUE,
          options = list(
            'placeholder' = 'Select some stations...',
            'plugins' = list('remove_button')
          )
        ),
        selectizeInput(
          inputId =  ns('parameters'),
          label = span('Parameters', actionButton(ns('allParameters'), 'All'), actionButton(ns('clearParameters'), 'Clear')),
          choices = c('BP', 'CO2atm', 'TURB', 'DO', 'CDOM', 'COND', 'PAR1', 'PAR2', 'DEPTH', 'pCO2'),
          multiple = TRUE,
          options = list(
            'placeholder' = 'Select some parameters...',
            'plugins' = list('remove_button')
          )
        )
      ),
      # Create the folders selection input
      div(
        class = 'left-btns',
        div(
          class = 'input-dir',
          shinyDirButton(ns('inputDir'), 'Input Directory', 'Choose input directory'),
          textOutput(ns('selectedInputDir'), container = p),
        ),
        div(
          class = 'input-dir',
          shinyDirButton(ns('outputDir'), 'Output Directory', 'Choose output directory'),
          textOutput(ns('selectedOutputDir'), container = p),
        )
      ),
      # Create the fake R console
      pre(id = 'console-raw', class = 'console')
    )
  )
}


## Server function ################################################################

sensorRawDataProcessing <- function(input, output, session, roots) {
  ## Directories slection #######################################################

  # Run logic for the folder browsing windows
  shinyDirChoose(input, 'inputDir', roots = roots, defaultRoot = 'working directory')
  shinyDirChoose(input, 'outputDir', roots = roots, defaultRoot = 'working directory')

  # Render the selected folder path
  output$selectedInputDir <- renderText(parseDirPath(roots, input$inputDir))
  output$selectedOutputDir <- renderText(parseDirPath(roots, input$outputDir))


  ## Stations and Parameters selection ##########################################

  # Create observeEvents to update the select inputs with all the options
  observeEvent(input$allSites, ignoreInit = TRUE, {
    updateSelectizeInput(session, 'sites', selected = c('VAD', 'VAU', 'VEL', 'FED', 'FEU', 'PEU', 'AND', 'ANU', 'RIC', 'VID', 'VIM', 'VIU'))
  })
  observeEvent(input$allParameters, ignoreInit = TRUE, {
    updateSelectizeInput(session, 'parameters', selected = c('PAR1', 'PAR2', 'BP', 'CO2atm', 'COND', 'DO', 'CDOM', 'TURB', 'pCO2', 'DEPTH'))
  })

  # Create observeEvents to clear the select inputs
  observeEvent(input$clearSites, ignoreInit = TRUE, {
    updateSelectizeInput(session, 'sites', selected = '')
  })
  observeEvent(input$clearParameters, ignoreInit = TRUE, {
    updateSelectizeInput(session, 'parameters', selected = '')
  })



  ## Script running #############################################################

  # Create an observeEvent that run when the run button is pressed
  observeEvent(input$run, ignoreInit = TRUE, {
    # Ensure that all the inputs are present
    req(input$sites, input$parameters, parseDirPath(roots, input$inputDir), parseDirPath(roots, input$outputDir))

    # Start the spinner
    show_spinner()

    # Clear the console
    sendUpdateToConcole(id = 'console-raw', action = 'clear')

    # Create log and warining files
    logFile <- createLogFile(parseDirPath(roots, input$outputDir))
    warningFile <- createLogFile(parseDirPath(roots, input$outputDir), 'warnings')

    # Run the script and redirect the message and warning outputs tot the fake R cosnole
    withConsoleRedirect('console-raw', {
      combineSensorsDataPerSite(
        inputDir = parseDirPath(roots, input$inputDir),
        outputDir = parseDirPath(roots, input$outputDir),
        sites = input$sites,
        parameters = input$parameters
      )
    }, logFile, warningFile)

    # Stop spinner
    hide_spinner()
  })



  ## Help logic ###################################################################

  # Create observeEvent that react to the help button
  observeEvent(input$help, ignoreInit = TRUE, {
    showModal(
      modalDialog(
        title = 'HELP', size = 'l', easyClose = TRUE,
        htmlTemplate(system.file('html/combine_help.html', package = "metalpDataPreprocessing"))
      )
    )
  })
}

