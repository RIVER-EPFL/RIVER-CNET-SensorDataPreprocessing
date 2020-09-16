## Parse and combined raw sensors data together by sites

## Combine Data Function ############################################################

combineSensorsDataPerSite <- function(inputDir, outputDir, sites, parameters) {
  message('Input directory: ', inputDir)
  message('Output directory: ', outputDir)
  message()

  dirsList <- list.dirs(inputDir) %>% grep('/[[:alnum:]]+_[[:alpha:]]{3}$', ., value = TRUE)
  if (length(dirsList) == 0) stop('Input directory is empty...')
  message('Found ', length(dirsList), ' directory/ies to use.')
  message()

  for (site in sites) {
    message('Merging data for ', site, ' ...')
    siteDirsList <- grep(paste0('_', site, '$'), dirsList, ignore.case = TRUE, value = TRUE)
    if (length(siteDirsList) == 0) {
      message('\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
      warning(paste0('No directory found for ', site, '!'), call. = FALSE, immediate. = TRUE)
      message('!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n')
      next
    }
    siteData <- data.frame('Date' = POSIXct(0))
    for (parameter in parameters) {
      parameter <- parametersInfo %>% filter(name == parameter)
      parameterData <- data.frame()
      message('  Merging patameter ', parameter$name, ' ...')
      for (dir in siteDirsList){
        file <- list.files(dir, pattern = parameter$pattern, ignore.case = TRUE, full.names = TRUE, recursive = TRUE)
        if (parameter$pattern == 'Cat.TXT$') file %<>% grep(parameter$subpattern, ., ignore.case = TRUE, value = TRUE)
        if (length(file) == 0) next

        currentFunc <- match.fun(parameter$parsingFunc)

        fileData <- currentFunc(file)

        parameterData <- appendData(parameterData, fileData)
      }

      if (sum(dim(parameterData)) == 0) {
        message('\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
        warning(paste0('No file found for parameter ', parameter$name, '!'), call. = FALSE, immediate. = TRUE)
        message('!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n')
      } else {
        siteData %<>% full_join(parameterData, by = 'Date')
      }
    }

    siteData %<>% arrange(Date)

    outputFile <- file.path(outputDir, paste0(site, '_raw_hf.csv'))
    message('Saving ', site, ' data to: ', outputFile, ' ...', '\n')
    write.csv(siteData, outputFile, row.names = FALSE)
  }

  message('Done.')
}




## Parameter parsing functions ####################################################

parseBP <- function(filePath) {
  bpDf <- fread(filePath) %>%
    rename(Date = Time, BPmbar = `Chan 1 - milliBar`, BPTemp = `Chan 2 - Deg C`) %>%
    select(Date, BPmbar, BPTemp) %>%
    mutate(Date = mdy_hms(Date))

  parseDate(bpDf)
}


parseNDEPTH <- function(filePath) {
  ndepthDf <- fread(filePath)
  if ('wtemp_a_2' %in% colnames(ndepthDf)) {
    ndepthDf %<>% rename(
      Date = datetime,
      DepthTempdegC1 = wtemp_p_1,
      DepthTempdegC2 = wtemp_a_2,
      WaterDepthmm1 = wtrhgt__3,
      WaterDepthmm2 = wtrhgt__4,
      DepthBatt = batt_mi_5
    ) %>%
    select(Date, DepthTempdegC1, DepthTempdegC2, WaterDepthmm1, WaterDepthmm2, DepthBatt)
  } else {
    ndepthDf %<>% rename(
      Date = datetime,
      DepthTempdegC1 = wtemp_p_1,
      WaterDepthmm1 = wtrhgt__2,
      WaterDepthmm2 = wtrhgt__3,
      DepthBatt = batt_mi_4
    ) %>%
      select(Date, DepthTempdegC1, WaterDepthmm1, WaterDepthmm2, DepthBatt)
  }

  ndepthDf %<>% mutate(Date = dmy_hms(Date))

  parseDate(ndepthDf)
}


parseCO2ATM <- function(filePath) {
  co2atmDf <- fread(filePath, fill=TRUE)

  if (all(c('Power_On', 'Measurement_Start', 'Measurement_End') %in% pull(tail(co2atmDf, 5), Type))) {
    co2atmDf %<>% slice(-(n()-2))
  }

  co2atmDf %<>% rename(
      CO2atmppm = `CO2(ppm)`,
      CO2atmTemp = `Temp(\xb0C)`,
      CO2atmRH = `RH(%)`
    ) %>%
    filter(Type == 'Data') %>%
    select(Date, CO2atmppm, CO2atmTemp, CO2atmRH) %>%
    mutate(Date = ymd_hms(Date))

  if (co2atmDf$Date[1] > co2atmDf$Date[nrow(co2atmDf)]) {
    lastCorrectDate <- max(co2atmDf$Date)
    correctDates <- co2atmDf %>% filter(year(Date) == year(lastCorrectDate)) %>% pull(Date)
    nbIncorrectDates <- co2atmDf %>% filter(year(Date) != year(lastCorrectDate)) %>% nrow()
    dateIntervalMinutes <- int_length(correctDates[1] %--% correctDates[2]) / 60
    correctedDates <- seq.POSIXt(
      from = lastCorrectDate + minutes(dateIntervalMinutes),
      by = paste0(dateIntervalMinutes, ' mins'),
      length.out = nbIncorrectDates
    )
    message('\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
    warning(paste0('Date resetting problem for CO2ATM.\n  Starting at date: ', min(correctedDates), '\n  Finishing at date: ', max(correctedDates)), call. = FALSE, immediate. = TRUE)
    message('!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n')
    co2atmDf %<>% mutate(Date = all_of(c(correctDates, correctedDates)))
  }

  parseDate(co2atmDf)
}


parseCOND <- function(filePath) {
  condDf <- fread(filePath) %>%
    rename(
      Date = starts_with('Date'),
      ConduScm = starts_with('Low Range'),
      CondTemp = starts_with('Temp')
    ) %>%
    select(Date, ConduScm, CondTemp) %>%
    mutate(Date = mdy_hms(Date))

  parseDate(condDf)
}


parsePAR1 <- function(filePath) {
  par1Df <- fread(filePath) %>%
    rename(
      Date = starts_with('Date'),
      PAR1Lux = starts_with('Intensity'),
      PAR1Temp = starts_with('Temp')
    ) %>%
    select(Date, PAR1Lux, PAR1Temp) %>%
    mutate(Date = mdy_hms(Date))

  parseDate(par1Df)
}


parsePAR2 <- function(filePath) {
  par2Df <- fread(filePath) %>%
    rename(
      Date = starts_with('Date'),
      PAR2Lux = starts_with('Intensity'),
      PAR2Temp = starts_with('Temp')
    ) %>%
    select(Date, PAR2Lux, PAR2Temp) %>%
    mutate(Date = mdy_hms(Date))

  parseDate(par2Df)
}


parseCDOM <- function(filePath) {
  cdomDf <- fread(filePath) %>%
    rename(
      Date = `Central European Time`,
      CDOMBatt = Battery,
      CDOMTemp = Temperature,
      CDOMppb = Sensor,
      CDOMGain = Gain
    ) %>%
    slice(-1) %>%
    select(Date, CDOMBatt, CDOMTemp, CDOMppb, CDOMGain) %>%
    mutate(
      Date = ymd_hms(Date),
      across(c(CDOMBatt, CDOMTemp, CDOMppb), as.numeric),
      CDOMGain = as.integer(CDOMGain)
    )

  parseDate(cdomDf)
}


parseDO <- function(filePath) {
  doDf <- fread(filePath) %>%
    rename(
      Date = `Central European Time`,
      DOBatt = Battery,
      DOTemp = Temperature,
      DOmgL = `Dissolved Oxygen`,
      DOsat = `Dissolved Oxygen Saturation`,
      DOQ = Q
    ) %>%
    slice(-1) %>%
    select(Date, DOBatt, DOTemp, DOmgL, DOsat, DOQ) %>%
    mutate(
      Date = ymd_hms(Date),
      across(-Date, as.numeric)
    )

  parseDate(doDf)
}


parseTURB <- function(filePath) {
  turbDf <- fread(filePath) %>%
    rename(
      Date = `Central European Time`,
      TurbiBatt = Battery,
      TurbiTemp = Temperature,
      TurbiNTU = Sensor,
      TurbiGain = Gain
    ) %>%
    slice(-1) %>%
    select(Date, TurbiBatt, TurbiTemp, TurbiNTU, TurbiGain) %>%
    mutate(
      Date = ymd_hms(Date),
      across(c(TurbiBatt, TurbiTemp, TurbiNTU), as.numeric),
      TurbiGain = as.integer(TurbiGain)
    )

  parseDate(turbDf)
}


parsePCO2 <- function(filePath) {
  pco2Df <- fread(filePath) %>%
    rename(
      Date = starts_with('Date'),
      pCO2ppm = ppm
    ) %>%
    mutate(Date = dmy_hms(Date))

  parseDate(pco2Df)
}





## Date correction function #######################################################

parseDate <- function(df) {
  df %<>% distinct(Date, .keep_all = TRUE)
  # Set seconds to 00
  second(df$Date) <- 00
  # If starting date minutes are not dividable by ten
  # Round the minutes to the closer multiple of ten
  if (minute(df$Date[1]) %% 10 != 0) df$Date <- round_date(df$Date, '10 mins')
  # If intervals are bigger than 10min
  # Interpolate the missing data
  if (df$Date[1] %--% df$Date[nrow(df)] / minutes(10) + 1 != nrow(df)) {
    df <- data.frame('Date' = seq.POSIXt(from = df$Date[1], to = df$Date[nrow(df)], by = '10 mins')) %>%
      full_join(df, by = 'Date') %>%
      mutate(across(-Date, na.approx, maxgap = 2))
  }
  # Return updated df
  return(df)
}



## Row merging function ###########################################################

appendData <- function(df, newDf) {
  if(sum(dim(df)) == 0) return(newDf)
  endDateDf <- max(df$Date)
  startDateNewDf <- min(newDf$Date)

  if (startDateNewDf - endDateDf > minutes(10)) {
    df %<>% bind_rows(
      data.frame('Date' = seq.POSIXt(from = endDateDf + minutes(10), to = startDateNewDf - minutes(10), by = '10 mins')),
      newDf
    )
  } else if (startDateNewDf - endDateDf == minutes(10)) {
    df %<>% union(newDf)
  } else {
    df %<>% coalesce_join(newDf, by = 'Date')
  }

  return(df)
}

