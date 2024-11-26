## Module containing the Sensor Raw data processing to portal data format

## UI function #####################################################################

portalDataConversionUI <- function(id) {
  # Create namespace
  ns <- NS(id)

  tagList(
    # Create a title bar with a run button
    gadgetTitleBar(
      'Sensors data conversion for portal',
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
          choices = c('DGT', 'FP1', 'FP3', 'FP4', 'GLT', 'GLV', 'RGV'),
          multiple = TRUE,
          options = list(
            'placeholder' = 'Select some stations...',
            'plugins' = list('remove_button')
          )
        ),
        selectInput(
          inputId =  ns('dataToSave'),
          label = 'Data To Save',
          choices = list(
            "Choose one" = "",
            'The 10min data' = '10min',
            'The 6H data' = '6H',
            'The 12H data' = '12H',
            'The 24H data' = '24H',
            'The 6, 12 and 24H data' = 'hourly',
            'All the data' = 'all'
          )
        )
      ),
      # Create the folders selection input
      div(
        class = 'left-btns',
        div(
          class = 'input-dir',
          shinyDirButton(ns('inputMeasured'), 'Input Measured Directory', 'Choose measured directory'),
          textOutput(ns('selectedInputMeasured'), container = p),
        ),
        div(
          class = 'input-dir',
          shinyDirButton(ns('inputModeled'), 'Input Modeled Directory', 'Choose modeled directory'),
          textOutput(ns('selectedInputModeled'), container = p),
        ),
        div(
          class = 'input-dir',
          shinyDirButton(ns('outputDir'), 'Output Directory', 'Choose output directory'),
          textOutput(ns('selectedOutputDir'), container = p),
        )
      ),
      # Create the fake R console
      pre(id = 'console-portal', class = 'console')
    )
  )
}


## Server function ################################################################

portalDataConversion <- function(input, output, session, roots) {
  ## Directories slection #######################################################

  # Run logic for the folder browsing windows
  shinyDirChoose(input, 'inputMeasured', roots = roots, defaultRoot = 'working directory')
  shinyDirChoose(input, 'inputModeled', roots = roots, defaultRoot = 'working directory')
  shinyDirChoose(input, 'outputDir', roots = roots, defaultRoot = 'working directory')

  # Render the selected folder path
  output$selectedInputMeasured <- renderText(parseDirPath(roots, input$inputMeasured))
  output$selectedInputModeled <- renderText(parseDirPath(roots, input$inputModeled))
  output$selectedOutputDir <- renderText(parseDirPath(roots, input$outputDir))


  ## Stations and Parameters selection ##########################################

  # Create observeEvents to update the select inputs with all the options
  observeEvent(input$allSites, ignoreInit = TRUE, {
    updateSelectizeInput(session, 'sites', selected = c('DGT', 'FP1', 'FP3', 'FP4', 'GLT', 'GLV', 'RGV'))
  })

  # Create observeEvents to clear the select inputs
  observeEvent(input$clearSites, ignoreInit = TRUE, {
    updateSelectizeInput(session, 'sites', selected = '')
  })



  ## Script running #############################################################

  # Create an observeEvent that run when the run button is pressed
  observeEvent(input$run, ignoreInit = TRUE, {
    # Ensure that all the inputs are present
    req(input$sites, input$dataToSave, parseDirPath(roots, input$inputMeasured),
        parseDirPath(roots, input$inputModeled), parseDirPath(roots, input$outputDir))

    # Start the spinner
    show_spinner()

    # Clear the console
    sendUpdateToConcole(id = 'console-portal', action = 'clear')

    # Create log and warining files
    logFile <- createLogFile(parseDirPath(roots, input$outputDir))
    warningFile <- createLogFile(parseDirPath(roots, input$outputDir), 'warnings')

    # Run the script and redirect the message and warning outputs tot the fake R cosnole
    withConsoleRedirect('console-portal', {
      convertDataForPortal(
        inputMeasuredDir = parseDirPath(roots, input$inputMeasured),
        inputModeledDir = parseDirPath(roots, input$inputModeled),
        outputDir = parseDirPath(roots, input$outputDir),
        dataToSave = input$dataToSave,
        sites = input$sites
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
        htmlTemplate(system.file('html_templates/conversion_help.html', package = "metalpDataPreprocessing"))
      )
    )
  })
}

