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
    # Create a title bar with a run button
    gadgetTitleBar(
      'Sensors raw data processing',
      left = NULL,
      right = miniTitleBarButton('run', 'Run',primary = TRUE)
    ),
    # Create the content of the gadget as a grid
    div(
      class = 'content-wrapper grid-2',
      # Create the select inputs
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
      # Create the folders selection input
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
      # Create the fake R console
      pre(id = 'console-raw', class = 'console')
    )
  )


  ## Server function ##############################################################

  server <- function(input, output, session) {
    # Create the possible roots for the folder browsing window
    roots <- c('root' = '/', 'home' = '~', 'working directory' = '.')


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

      # Run the script and redirect the message and warning outputs tot the fake R cosnole
      withConsoleRedirect('console-raw', {
        combineSensorsDataPerSite(
          inputDir = parseDirPath(roots, input$inputDir),
          outputDir = parseDirPath(roots, input$outputDir),
          sites = input$sites,
          parameters = input$parameters
        )
      })

      # Stop spinner
      hide_spinner()
    })
  }

  viewer <- dialogViewer('METALP Data Preprocessing', width = 800, height = 800)
  # viewer <- browserViewer()
  runGadget(ui, server, viewer = viewer)
}
