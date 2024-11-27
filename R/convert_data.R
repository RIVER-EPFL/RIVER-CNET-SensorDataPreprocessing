## Merging of the modeled and measured data           #############################
## Calculation of the the 24, 12 and 6 hours interval #############################
## And formatting data for the portal                 #############################

## Function to create the interval data ############################################

convertDataForPortal <- function(inputMeasuredDir, inputModeledDir, outputDir, dataToSave, sites) {
  # Create new interval data with individual site data files
  # Parameters:
  #  - inputMeasuredDir: String, the input directory to take the the measured data from
  #  - inputModeledDir: String, the input directory to take the the modeled data from
  #  - outputDir: String, the output directory to write the data to
  #  - dataToSave: Vector, indicates which data to save. Must contains one of or a combination of the following string:
  #                - '10min': The 10min data
  #                - '6H': The 6H data
  #                - '12H': The 12H data
  #                - '24H': The 24H data
  #                - 'hourly': The 6, 12 and 24H data
  #                - 'all': All the data, default
  #  - sites: Character Vector, the sites to use for convertion
  #
  # Returns NULL

  ## Loading the 10 minutes data files ###############################################

  # Indicate the selected input and output directories paths
  message('Input measured directory: ', inputMeasuredDir)
  message('Input modeled directory: ', inputModeledDir)
  message('Output directory: ', outputDir)
  message()

  # List directories
  # If either one of them is empty, abort
  filesPerDir <- list(
    'measured' = listSitesFiles(inputMeasuredDir),
    'modeled' = listSitesFiles(inputModeledDir)
  )
  if (isFALSE(filesPerDir$measured) | isFALSE(filesPerDir$modeled)) {
    message('\nAbort.')
    return()
  }

  message('Loading data ...')

  # Create empty df for 10min data
  hf_10min_df <- data.frame()

  # For each selected site
  for (site in sites) {
    message('  Loading ', site, ' data ...')

    # Get data
    dataList <- getSiteFilesData(filesPerDir, site)

    # If failed go to next site
    if (isFALSE(dataList)) next

    # Merge data and append to 10min df
    message('  Merging measured and modeled data for ', site, ' ...')

    hf_10min_df %<>%  bind_rows(mergeMeasuredAndModeled(dataList$measured, dataList$modeled, site))
  }


  # If no 10min data, warn and abort
  if (nrow(hf_10min_df) == 0) {
    warningMessage('The 10 minutes data frame is empty!')
    message('\nAbort.')
    return()
  }


  message()
  message('Parsing data...')
  message('  Converting Date to POSIXct...')

  # Convert date to POSIXct
  hf_10min_df$Date %<>% ymd_hms(tz = 'GMT')


  message('  Converting data_type to factor...')

  # Convert data_type to factor
  hf_10min_df$data_type %<>% as.factor()


  message('  Converting Site_ID to factor...')

  # Convert Site_ID to factor
  hf_10min_df$Site_ID %<>% as.factor()


  message('  Pivoting wider the data...')

  # Transforme df to wide format
  hf_10min_df %<>% pivot_wider(
	id_cols = c(Date, Site_ID, data_type), # Explicitly naming `id_cols`
    names_from = data_type,
    values_from = where(is.numeric),
    names_glue = "{.value}_{data_type}"
  )


  message('  Adding single points info...')

  # Add single points info for each parameter
  hf_10min_df %<>% addSinglePointsInfo()


  message()
  message('HF 10min data ready!')
  message()

  if ('10min' %in% dataToSave | 'all' %in% dataToSave) {
    # Create outfile path
    outFile <- file.path(outputDir, '10min_data.csv')

    message('Saving HF 10min data in ', outFile, ' ...')

    # Save the 10min data
    write.csv(hf_10min_df, file = outFile, row.names = FALSE)

    message('HF 10min data saved!')
    message()
  }


  ## Computing 6, 12 and 14 hours data ###############################################

  if ('6H' %in% dataToSave | 'hourly' %in% dataToSave | 'all' %in% dataToSave) {

    message('Computing 6H data...')

    # Create the 6H data
    hf_6H_df <- hf_10min_df %>% movingAverage(daily = FALSE, intLength = 6)

    message('HF 6H data ready!')
    message()

    # Create outfile path
    outFile <- file.path(outputDir, '6H_data.csv')

    message('Saving HF 6H data in ', outFile, ' ...')

    # Save the 6H data
    write.csv(hf_6H_df, file = file.path(outputDir, '6H_data.csv'), row.names = FALSE)

    rm(hf_6H_df)

    message('HF 6H data saved!')
    message()
  }

  if ('12H' %in% dataToSave | 'hourly' %in% dataToSave | 'all' %in% dataToSave) {

    message('Computing 12H data...')

    # Create the 12H data
    hf_12H_df <- hf_10min_df %>% movingAverage(daily = FALSE, intLength = 12)

    message('HF 12H data ready!')
    message()

    # Create outfile path
    outFile <- file.path(outputDir, '12H_data.csv')

    message('Saving HF 12H data in ', outFile, ' ...')

    # Save the 12H data
    write.csv(hf_12H_df, file = file.path(outputDir, '12H_data.csv'), row.names = FALSE)

    rm(hf_12H_df)

    message('HF 12H data saved!')
    message()
  }

  if ('24H' %in% dataToSave | 'hourly' %in% dataToSave | 'all' %in% dataToSave) {

    message('Computing 24H data...')

    # Create the 24H data
    hf_24H_df <- hf_10min_df %>% movingAverage()

    message('HF 24H data ready!')
    message()

    # Create outfile path
    outFile <- file.path(outputDir, '24H_data.csv')

    message('Saving HF 24H data in ', outFile, ' ...')

    # Save the 24H data
    write.csv(hf_24H_df, file = file.path(outputDir, '24H_data.csv'), row.names = FALSE)

    rm(hf_24H_df)

    message('HF 24H data saved!')
    message()
  }

  message()
  message('Done.')
}




## Functions to manipulate files ##################################################

listSitesFiles <- function(dir, filePattern = '[[:alnum:]_]*[[:upper:]]{3}[[:alnum:]_]*\\.csv$') {
  # List files using pattern
  dirFiles <- list.files(dir, pattern = filePattern, full.names = TRUE)

  # Check if empty
  if (length(dirFiles) == 0) {
    warningMessage(paste0('No file found in the following directory:\n',
                          '  ', dir))
    return(FALSE)
  }

  # Return file list
  dirFiles
}


getSiteFilesData <- function(filesPerDir, site,
                             columnsRef = c('Date', 'BPmbar', 'CDOMppb',
                                            'CO2atmppm', 'ConduScm', 'DOmgL',
                                            'WaterDepthmm', 'PAR1Lux', 'PAR2Lux',
                                            'TurbiNTU', 'WaterTempdegC', 'pCO2ppm')) {
  # removed 'DischargeLsec' from the list, replaced by 'WaterDepthmm'
  # filesPerDir: Named list of character vectors, names -> directory name, vector -> files
  # site: String, site name
  # columnsRef: Character vector, columns to check for presence

  # Create search pattern
  filePattern <- paste0('/[[:alnum:]_]*', site, '[[:alnum:]_]*\\.csv$')

  # Create empty list of files
  siteFiles <- list()

  # Find files for each data type
  for (dir in names(filesPerDir)) {
    # Find file
    currentFile <- grep(filePattern, filesPerDir[[dir]], value = TRUE)

    # If no file, warn user and return
    if (length(currentFile) == 0) {
      warningMessage(paste0('No ', dir, ' file found for ', site, '!'))
      return(FALSE)
    }

    # If more than one file, warn user and return
    if (length(currentFile) > 1) {
      warningMessage(paste0('Too many ', dir, ' files found for ', site, '!'))
      return(FALSE)
    }

    # If file is empty, warn and return
    if (file.info(currentFile)$size == 0) {
      warningMessage(paste0('The ', dir, ' file for ', site, ' is empty!\n',
                            '  File: ', currentFile))
      return(FALSE)
    }

    # Load data
    fileData <- fread(currentFile)

    # If df is empty, warn and return
    if (nrow(fileData) == 0) {
      warningMessage(paste0('The ', dir, ' file for ', site, ' is an empty data frame!\n',
                            '  File: ', currentFile))
      return(FALSE)
    }

    # If df has not the correct columns, warn and return
    columns <- colnames(fileData)
    if ((length(columns) != length(columnsRef) | (!all(columns %in% columnsRef)))) {
      warningMessage(paste0('The ', dir, ' file for ', site, ' does not have the correct columns!\n',
                            '  File: ', currentFile, '\n',
                            '  Correct columns names are: ', paste(columnsRef, collapse = ', ')))
      return(FALSE)
    }

    # Save data into list
    siteFiles[[dir]] <- fileData
  }

  # If the two data frame have not the same length, warn and return
  if (nrow(siteFiles$measured) != nrow(siteFiles$modeled)) {
    warningMessage(paste0('The two data frame form the files of ', site, ' have not the same length!'))
    return(FALSE)
  }

  siteFiles
}



## Functions to combine measured and modeled data ##################################

mergeMeasuredAndModeled <- function(measuredData, modeledData, site) {
  # Pivot longer measured and modeled data
  	measuredData %<>% pivot_longer(
	cols = -Date, # Columns to pivot
	names_to = 'parameter', # New column for parameter names
	values_to = 'measured', # New column for measured values
	id_cols = c(Date) # Explicitly naming `id_cols`
	)

	modeledData %<>% pivot_longer(
	cols = -Date, # Columns to pivot
	names_to = 'parameter', # New column for parameter names
	values_to = 'modeled', # New column for modeled values
	id_cols = c(Date) # Explicitly naming `id_cols`
	)

# Return directly the output
  # Join the data
  full_join(measuredData, modeledData, by = c('Date', 'parameter')) %>%
    # Remove modeled data where there is a measured one
    mutate(
      modeled = if_else(!is.na(measured), as.numeric(NA), modeled),
      Site_ID = site
    ) %>%
    # **Updated `pivot_longer()` with explicit `id_cols`**
    pivot_longer(
      cols = c(measured, modeled),    # Columns to pivot
      names_to = 'data_type',         # New column for data types
      values_to = 'value',            # New column for values
      id_cols = c(Date, Site_ID)      # Explicitly naming `id_cols`
    ) %>%
    # **Updated `pivot_wider()` with explicit `id_cols`**
    pivot_wider(
      id_cols = c(Date, Site_ID, data_type),  # Explicitly naming `id_cols`
      names_from = 'parameter',                # Columns to pivot names from
      values_from = 'value',                   # Columns to pivot values from
      names_glue = "{.value}_{data_type}"      # New column names format
    ) %>%
    # Round to 2 decimals
    mutate(across(where(is.numeric), round, digits = 2))
}



## Functions to compute intervals mean ##############################################

movingAverage <- function(df, daily = TRUE, intLength = NULL,
                          dateCol = 'Date', siteCol = 'Site_ID',
                          parameters = NULL, mergeModeled = TRUE) {
  # Function that compute the daily average of a parameter
  # Parameters:
  #  - df: Data.frame, the data to perform the moving average onto
  #  - daily: boolean, if TRUE do the daily average, else use intLength for the moving average
  #  - intLength: integer, the intervals length in hours, default NULL
  #               Needed if daily is FALSE
  #  - dateCol: String, the df column name containing the date, default: 'Date'
  #  - siteCol: String, the df column name containing the site info, default: 'Site_ID',
  #  - parameters: Vector of String, the parameters of the df, default: NULL
  #               If NULL, all df columns that are not dateCol or siteCol will be used to get the parameters names
  #  - mergeModeled: Boolean, indicates if the average need to be done on the merged data (measured and modeled) or separately, default: TRUE
  #
  # Returns a data.frame with the daily or specified moving average

  # Remove all singlePoint info columns
  df %<>% select(-ends_with('singlePoint'))

  # If valueCols is NULL get all the remaining columns of the df
  if (is.null(parameters)) {
    parameters <- df %>% select(-all_of(c(dateCol, siteCol))) %>% colnames() %>%
      gsub('(.*)_.*$', '\\1', .) %>% unique()
  }

  # Merge measured and modeled data if needed
  if (mergeModeled) {
    # For each parameter merge the measured and modeled data
    for (column in parameters) {
      tmpDf <- df %>% select(starts_with(column))
      colsToRemove <- colnames(tmpDf)
      df %<>% mutate(!!column := rowSums(tmpDf, na.rm=TRUE) * NA ^ !rowSums(!is.na(tmpDf))) %>%
        select(-c(all_of(colsToRemove)))
    }

    # Delete tmp variables
    rm(tmpDf, colsToRemove)
  }

  # Update dateCol by smoothing the date interval
  message('  Reducing interval ...')

  if (daily) {
    # Keep only the date
    df %<>% mutate(!!dateCol := date(!!sym(dateCol)))
  } else {
    # Create new custom interval
    df %<>% reduceDateInterval(dateCol, intLength)
  }

  # Make the average of the newly made intervals
  message('  Computing the average ...')

  df %<>% group_by_at(c(dateCol, siteCol)) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
    mutate(across(where(is.numeric), round, digits = 2))

  # Return the new df
  return(df)
}


reduceDateInterval <- function(df, dateCol, intLength, timePointsInt = 10) {
  # Create a new df with an update date column containing bigger interval, need to average values afterwards
  # Parameters:
  #  - df: Data.frame, the data to update the date from
  #  - dateCol: String, the df column name containing the date
  #  - intLength: integer, the intervals length in hours
  #  - timePointsInt: integer, the interval in minutes between the time points in the dateVector, default 10
  #
  # Retruns an updated df

  # Get the global interval
  intGlobal <- interval(min(pull(df, dateCol)), max(pull(df, dateCol)))
  # Get the number of complete intervals
  nInt <- intGlobal %/% hours(intLength)
  # Create an empty var to store the previous interval
  previousInt <- NULL

  # Create nInt intervals and store them in intsList
  for (i in c(1:nInt)) {
    # If it is the first interval take the intGlobal start as starting date
    # Otherwise take the previous interval end plus timePointsInt as starting date
    if (i == 1) {
      start <- int_start(intGlobal)
    } else {
      start <- int_end(previousInt) + minutes(timePointsInt)
    }
    # Set the ending date using the intLength minus timePointsInt
    # And create the interval
    end <- start + hours(intLength) - minutes(timePointsInt)
    currentInt <- interval(start, end)

    # Update df dateCol values using the interval
    df %<>% mutate(!!dateCol := if_else(!!sym(dateCol) %within% currentInt, int_start(currentInt), !!sym(dateCol)))

    # Set previousInt to currentInt
    previousInt <- currentInt
  }

  # If the intGlobal is not dividable by the intLength
  # Add the remaining incomplete interval
  if (int_length(intGlobal %% hours(intLength)) != 0) {
    # Set the starting date as the last interval end plus timePointsInt
    start <- int_end(previousInt) + minutes(timePointsInt)
    # Set the ending date as the intGlobal end plus the timePointsInt
    # And create the interval
    end <- int_end(intGlobal)
    currentInt <- interval(start, end)

    # Update df dateCol values using the interval
    df %<>% mutate(!!dateCol := if_else(!!sym(dateCol) %within% currentInt, int_start(currentInt), !!sym(dateCol)))
  }

  # Return the updated df
  return(df)
}



addSinglePointsInfo <- function(df, colSuffix = 'singlePoint') {
  # Get parameters names
  parameters <- df %>% select(where(is.numeric)) %>% colnames() %>%
    gsub('(.*)_.*$', '\\1', .) %>% unique()

  # Convert df to data.table
  df %<>% as.data.table()

  # Create new df with only the date and site info
  newDf <- df %>% select(-where(is.numeric))

  # For each parameter determine every single points
  for (parameter in parameters) {
    # Create single point column name
    singlePointCol <- paste0(parameter, '_', colSuffix)
    # Subset the df to keep only parameter related data and add the single point column
    subdf <- df %>% select(starts_with(parameter)) %>% mutate(!!singlePointCol := as.factor(rep(0, nrow(df))))
    # Get the parameter measured column name
    measuredCol <- colnames(subdf)[1]

    # The first previous point is not NA (as it is NULL)
    previousIsNA <- FALSE
    # For each data point of the current parameter, determine if it is a single point
    for (i in c(1:nrow(subdf))) {
      # Determine if the current measured point is NA
      currentIsNA <- is.na(subdf[[measuredCol]][i])
      # If the current and previous points have different types (i.e. one is NA the other is a number)
      if (previousIsNA != currentIsNA) {
        # And if the current and previous points have different types
        if (currentIsNA != is.na(subdf[[measuredCol]][i+1])) {
          # Set the single point value to 1
          subdf[i, (singlePointCol) := as.factor(1)]
        }
      }
      # Set the previous point type as the current point type
      previousIsNA <- currentIsNA
    }
    # Bind columns of the newDf and the current updated subdf
    newDf <- bind_cols(newDf, subdf)
  }

  # Return the new df
  return(newDf)
}

