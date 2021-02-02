## code to prepare `parametersInfo`
## Sensors data parameters info used to look for and combine the different output files together

parametersInfo <- data.frame(
  'name' = c('BP', 'CO2atm', 'TURB', 'DO', 'CDOM', 'COND', 'PAR1', 'PAR2', 'DEPTH', 'pCO2'),
  'pattern' = c('_BP.csv$', '_CO2ATM.LOG$', 'Cat.TXT$', 'Cat.TXT$', 'Cat.TXT$', '_COND.csv$', '_PAR1.csv$', '_PAR2.csv$', '_NDEPTH.csv$', '_PCO2.csv$'),
  'subpattern' = c('NA', 'NA', '/TURB/', '/DO/', '/CDOM/', 'NA', 'NA', 'NA', 'NA', 'NA'),
  'parsingFunc' = c('parseBP', 'parseCO2ATM', 'parseTURB', 'parseDO', 'parseCDOM', 'parseCOND', 'parsePAR1', 'parsePAR2', 'parseNDEPTH', 'parsePCO2'),
  'columns' = c(
    'BPmbar,BPTemp',
    'CO2atmppm,CO2atmTemp,CO2atmRH',
    'TurbiBatt,TurbiTemp,TurbiNTU,TurbiGain',
    'DOBatt,DOTemp,DOmgL,DOsat,DOQ',
    'CDOMBatt,CDOMTemp,CDOMppb,CDOMGain',
    'ConduScm,CondTemp',
    'PAR1Lux,PAR1Temp',
    'PAR2Lux,PAR2Temp',
    'DepthTempdegC,WaterDepthmm,DepthBatt',
    'pCO2ppm'
  )
)

usethis::use_data(parametersInfo, overwrite = TRUE, internal = TRUE)
