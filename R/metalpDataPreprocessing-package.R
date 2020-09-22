#' METALP Sensor Data Preprocessing
#'
#' Use this addin to perform all the data preprocessing for the METALP sensors data
#'
#' @name metalpDataPreprocessing
#' @docType package
#' @import shiny miniUI lubridate magrittr dplyr
#' @importFrom data.table fread
#' @importFrom jsonlite toJSON
#' @importFrom purrr map_dfc
#' @importFrom zoo na.approx
#' @importFrom shinyFiles shinyDirButton shinyDirChoose parseDirPath
#' @importFrom shinybusy use_busy_spinner show_spinner hide_spinner
NULL
