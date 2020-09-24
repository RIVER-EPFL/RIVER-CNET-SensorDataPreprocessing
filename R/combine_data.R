## Parse and combined raw sensors data together by sites

## Combine Data Function ############################################################

combineSensorsDataPerSite <- function(inputDir, outputDir, sites, parameters) {
  # Indicate the selected input and output directories paths
  message('Input directory: ', inputDir)
  message('Output directory: ', outputDir)
  message()

  # List all directories within inputDir that have the following format: 00000000_AAA
  dirsList <- list.dirs(inputDir, recursive = FALSE) %>% grep('/[[:digit:]]{8}_[[:upper:]]{3}$', ., value = TRUE)

  # If no directory found abort
  if (length(dirsList) == 0) {
    warning('Input directory does not contain required directory structure...', call. = FALSE, immediate. = TRUE)
    message('\nAbort.')
    return()
  }

  # Display the number of found directories
  message('Found ', length(dirsList), ' directory/ies to use.')
  message()

  # For each site in the sites vector
  for (site in sites) {
    # Indicate the current processed site
    message('Merging data for ', site, ' ...')
    # Filter the directory list for the current site
    siteDirsList <- grep(paste0('_', site, '$'), dirsList, ignore.case = TRUE, value = TRUE)

    # Indicate if no directory was found for this site and pass to the next
    if (length(siteDirsList) == 0) {
      warningMessage(paste0('No directory found for ', site, '!'))
      next
    }

    # Create an empty data frame for the current site data
    # Create the an empty Date column for future joining
    siteData <- data.frame('Date' = POSIXct(0))

    # For each parameter in the parameters vector
    for (parameter in parameters) {
      # Retrieve the parameter info from the parametersInfo df (from sysdata)
      parameter <- parametersInfo %>% filter(name == parameter)
      # Create an empty data frame for the current parameter data
      parameterData <- data.frame()

      # Indicate that the curretn parameter is being processed
      message('  Merging parameter ', parameter$name, ' ...')

      # For each directory of the current site
      for (dir in siteDirsList){
        # Get the file corresponding to the current parameter in the current directory
        file <- list.files(dir, pattern = parameter$pattern, ignore.case = TRUE, full.names = TRUE, recursive = TRUE)

        # If the file is a Cat.TXT file present in a subfolder
        # Filter the results to get the correct one
        if (parameter$pattern == 'Cat.TXT$') file %<>% grep(parameter$subpattern, ., ignore.case = TRUE, value = TRUE)

        # If no file was found go to the next directory
        if (length(file) == 0) next

        # If the file is empty, warn the user and go to the next directory
        if (file.info(file)$size == 0) {
          warningMessage(paste0('The following file is empty!\n',
                                '  ', file))
          next
        }

        # Get the parsing function relative to the parameter
        currentFunc <- match.fun(parameter$parsingFunc)

        # Parse the file and append it to the current parameter data
        fileData <- currentFunc(file)
        parameterData <- appendData(parameterData, fileData)

      } # End of siteDirsList loop

      # If the current parameter data is empty
      # Warn the user and go for the next parameter
      # Else, append the data to the current site data
      if (sum(dim(parameterData)) == 0) {
        warningMessage(paste0('No file found for parameter ', parameter$name, '!'))
      } else {
        siteData %<>% full_join(parameterData, by = 'Date')
      }

    } # End of parameters loop

    # Reorder the site data ascending Date order
    siteData %<>% arrange(Date)

    # Create output file name and path, indicated saving process and write current site data
    outputFile <- file.path(outputDir, paste0(site, '_raw_hf.csv'))
    message('Saving ', site, ' data to: ', outputFile, ' ...', '\n')
    write.csv(siteData, outputFile, row.names = FALSE)

  } # End of sites loop

  # Indicate that the function completed successfully
  message('Done.')
}




## Parameter parsing functions ####################################################

# All the following functions takes a file path as string and returns a data frame


parseBP <- function(filePath) {
  # Read file
  bpDf <- fread(filePath) %>%
    # Rename the columns
    rename(Date = Time, BPmbar = `Chan 1 - milliBar`, BPTemp = `Chan 2 - Deg C`) %>%
    # Select the needed columns
    select(Date, BPmbar, BPTemp) %>%
    # Convert date column to POSIXct type
    mutate(Date = mdy_hms(Date))

  # If the df is empty return it
  if (nrow(bpDf) == 0) return(bpDf)

  # Parse date and return the output
  parseDate(bpDf)
}


parseNDEPTH <- function(filePath) {
  # Read file
  ndepthDf <- fread(filePath) %>% rename(
  # Rename the columns
    Date = datetime,
    DepthTempdegC = starts_with('wtemp'),
    WaterDepthmm = starts_with('wtrhgt'),
    DepthBatt = starts_with('batt')
  )

  # If the df is empty return it
  if (nrow(ndepthDf) == 0) return(ndepthDf)

  # If there are two columns for the temperature or depth
  # Rename the first one to keep only this one
  if ('DepthTempdegC1' %in% colnames(ndepthDf)) ndepthDf %<>% rename(DepthTempdegC = DepthTempdegC1)
  if ('WaterDepthmm1' %in% colnames(ndepthDf)) ndepthDf %<>% rename(WaterDepthmm = WaterDepthmm1)

  # Select the needed columns
  ndepthDf %<>% select(Date, DepthTempdegC, WaterDepthmm, DepthBatt) %>%
    # Convert date column to POSIXct type
    mutate(Date = dmy_hms(Date))


  # Parse date and return the output
  parseDate(ndepthDf)
}


parseCO2ATM <- function(filePath) {
  # Read file
  co2atmDf <- fread(filePath, fill=TRUE)

  # If there is an isolated measurement at the end remove it
  if (all(c('Power_On', 'Measurement_Start', 'Measurement_End') %in% pull(tail(co2atmDf, 5), Type))) {
    co2atmDf %<>% slice(-(n()-2))
  }

  # Rename the columns
  co2atmDf %<>% rename(
      CO2atmppm = `CO2(ppm)`,
      CO2atmTemp = `Temp(\xb0C)`,
      CO2atmRH = `RH(%)`
    ) %>%
    # Filter the data to keep only the data type
    # And meaningful data
    filter(
      Type == 'Data',
      CO2atmppm < 10000
    ) %>%
    # Select the needed columns
    select(Date, CO2atmppm, CO2atmTemp, CO2atmRH) %>%
    # Convert date column to POSIXct type
    # And create a CO2atmDateReset columns filled with 0
    mutate(
      Date = ymd_hms(Date),
      CO2atmDateReset = 0
    )

  # If all the Dates are from 2000
  # Warn the user and return an empty data frame
  if (all(year(co2atmDf$Date) == 2000)) {
    warningMessage(paste0('The following file is ignored because all the dates are from 2000.\n',
                          '  File: ', filePath))
    return(data.frame())
  }

  # If the df is empty return it
  if (nrow(co2atmDf) == 0) return(co2atmDf)

  # If the first date entry is older than the last one
  # Extrapolate the dates from the last meaningful date
  if (co2atmDf$Date[1] > co2atmDf$Date[nrow(co2atmDf)]) {
    # Indicate which data point for which the date will be corrected
    co2atmDf %<>% mutate(CO2atmDateReset = if_else(year(Date) == 2000, 1, CO2atmDateReset))

    # Get the number of incorrect dates and the interval between two dates in minutes
    nbIncorrectDates <- co2atmDf %>% filter(year(Date) == 2000) %>% nrow()
    dateIntervalMinutes <- round(int_length(co2atmDf$Date[1] %--% co2atmDf$Date[2]) / 60)

    # Create a vector of corrected dates:
    #  from: the last correct dates + the interval between two dates in minutes
    #  by: the interval between two dates in minutes
    #  of size: the number of incorrect dates
    correctedDates <- seq.POSIXt(
      from = max(co2atmDf$Date) + minutes(dateIntervalMinutes),
      by = paste0(dateIntervalMinutes, ' mins'),
      length.out = nbIncorrectDates
    )

    # Warn the user of this change
    warningMessage(paste0('Date resetting problem for CO2ATM.\n',
                          '  File: ', filePath, '\n',
                          '  Starting at date: ', min(correctedDates), '\n',
                          '  Finishing at date: ', max(correctedDates)))

    # Get the correct dates and merge it with the corrected dates to update the data frame
    correctDates <- co2atmDf %>% filter(year(Date) != 2000) %>% pull(Date)
    co2atmDf %<>% mutate(Date = all_of(c(correctDates, correctedDates)))
  }

  # Parse date and return the output
  parseDate(co2atmDf)
}


parseCOND <- function(filePath) {
  # Read file
  condDf <- fread(filePath) %>%
    # Rename the columns
    rename(
      Date = starts_with('Date'),
      ConduScm = starts_with('Low Range'),
      CondTemp = starts_with('Temp')
    ) %>%
    # Select the needed columns
    select(Date, ConduScm, CondTemp) %>%
    # Convert date column to POSIXct type
    mutate(Date = mdy_hms(Date))

  # If the df is empty return it
  if (nrow(condDf) == 0) return(condDf)

  # Parse date and return the output
  parseDate(condDf)
}


parsePAR1 <- function(filePath) {
  # Read file
  par1Df <- fread(filePath) %>%
    # Rename the columns
    rename(
      Date = starts_with('Date'),
      PAR1Lux = starts_with('Intensity'),
      PAR1Temp = starts_with('Temp')
    ) %>%
    # Select the needed columns
    select(Date, PAR1Lux, PAR1Temp) %>%
    # Convert date column to POSIXct type
    mutate(Date = mdy_hms(Date))

  # If the df is empty return it
  if (nrow(par1Df) == 0) return(par1Df)

  # Parse date and return the output
  parseDate(par1Df)
}


parsePAR2 <- function(filePath) {
  # Read file
  par2Df <- fread(filePath) %>%
    # Rename the columns
    rename(
      Date = starts_with('Date'),
      PAR2Lux = starts_with('Intensity'),
      PAR2Temp = starts_with('Temp')
    ) %>%
    # Select the needed columns
    select(Date, PAR2Lux, PAR2Temp) %>%
    # Convert date column to POSIXct type
    mutate(Date = mdy_hms(Date))

  # If the df is empty return it
  if (nrow(par2Df) == 0) return(par2Df)

  # Parse date and return the output
  parseDate(par2Df)
}


parseCDOM <- function(filePath) {
  # Read file
  cdomDf <- fread(filePath) %>%
    # Rename the columns
    rename(
      Date = `Central European Time`,
      CDOMBatt = Battery,
      CDOMTemp = Temperature,
      CDOMppb = Sensor,
      CDOMGain = Gain
    ) %>%
    # Remove the first row
    slice(-1) %>%
    # Select the needed columns
    select(Date, CDOMBatt, CDOMTemp, CDOMppb, CDOMGain) %>%
    # Convert date column to POSIXct type
    # The gain to integer type and the rest to numeric type
    mutate(
      Date = ymd_hms(Date),
      across(c(CDOMBatt, CDOMTemp, CDOMppb), as.numeric),
      CDOMGain = as.integer(CDOMGain)
    )

  # If the df is empty return it
  if (nrow(cdomDf) == 0) return(cdomDf)

  # Parse date and return the output
  parseDate(cdomDf)
}


parseDO <- function(filePath) {
  # Read file
  doDf <- fread(filePath) %>%
    # Rename the columns
    rename(
      Date = `Central European Time`,
      DOBatt = Battery,
      DOTemp = Temperature,
      DOmgL = `Dissolved Oxygen`,
      DOsat = `Dissolved Oxygen Saturation`,
      DOQ = Q
    ) %>%
    # Remove first row
    slice(-1) %>%
    # Select the needed columns
    select(Date, DOBatt, DOTemp, DOmgL, DOsat, DOQ) %>%
    # Convert date column to POSIXct type
    # The rest to numeric type
    mutate(
      Date = ymd_hms(Date),
      across(-Date, as.numeric)
    )

  # If the df is empty return it
  if (nrow(doDf) == 0) return(doDf)

  # Parse date and return the output
  parseDate(doDf)
}


parseTURB <- function(filePath) {
  # Read file
  turbDf <- fread(filePath) %>%
    # Rename the columns
    rename(
      Date = `Central European Time`,
      TurbiBatt = Battery,
      TurbiTemp = Temperature,
      TurbiNTU = Sensor,
      TurbiGain = Gain
    ) %>%
    # Remove the first row
    slice(-1) %>%
    # Select the needed columns
    select(Date, TurbiBatt, TurbiTemp, TurbiNTU, TurbiGain) %>%
    # Convert date column to POSIXct type
    # The gain to integer type and the rest to numeric type
    mutate(
      Date = ymd_hms(Date),
      across(c(TurbiBatt, TurbiTemp, TurbiNTU), as.numeric),
      TurbiGain = as.integer(TurbiGain)
    )

  # If the df is empty return it
  if (nrow(turbDf) == 0) return(turbDf)

  # Parse date and return the output
  parseDate(turbDf)
}


parsePCO2 <- function(filePath) {
  # Read file
  pco2Df <- fread(filePath) %>%
    # Rename the columns
    rename(
      Date = starts_with('Date'),
      pCO2ppm = ppm
    ) %>%
    # Convert date column to POSIXct type
    mutate(Date = dmy_hms(Date))

  # If the df is empty return it
  if (nrow(pco2Df) == 0) return(pco2Df)

  # Parse date and return the output
  parseDate(pco2Df)
}





## Date correction function #######################################################

parseDate <- function(df) {
  df %<>% distinct(Date, .keep_all = TRUE)
  # Set seconds to 00
  second(df$Date) <- 00
  # If starting date minutes are not dividable by ten
  # Round the minutes to the closer multiple of ten
  df$Date <- round_date(df$Date, '10 mins')
  # If intervals are bigger than 10min
  # Interpolate the missing data
  if (df$Date[1] %--% df$Date[nrow(df)] / minutes(10) + 1 != nrow(df)) {
    df <- data.frame('Date' = seq.POSIXt(from = df$Date[1], to = df$Date[nrow(df)], by = '10 mins')) %>%
      full_join(df, by = 'Date') %>%
      mutate(across(-Date, na.approx, maxgap = 5))
  }
  # Return updated df
  return(df)
}



## Row merging function ###########################################################

appendData <- function(df, newDf) {
  # If the newDf is empty return the old
  # And Vice-Versa
  if (nrow(newDf) == 0) return(df)
  if (sum(dim(df)) == 0) return(newDf)

  # Get the joining dates
  endDateDf <- max(df$Date)
  startDateNewDf <- min(newDf$Date)

  # If the interval between the joining dates is bigger than 10 minutes
  # Fill the time serie gap and fill the data of NAs
  if (startDateNewDf - endDateDf > minutes(10)) {
    df %<>% bind_rows(
      data.frame('Date' = seq.POSIXt(from = endDateDf + minutes(10), to = startDateNewDf - minutes(10), by = '10 mins')),
      newDf
    )
    # Else if the interval is exactly 10 minutes
    # Join the two data frame
  } else if (startDateNewDf - endDateDf == minutes(10)) {
    df %<>% union(newDf)
    # Otherwise perform a coalescent join to complete all missing data between the two data frame
  } else {
    df %<>% coalesce_join(newDf, by = 'Date')
  }

  # Return the updated df
  return(df)
}

