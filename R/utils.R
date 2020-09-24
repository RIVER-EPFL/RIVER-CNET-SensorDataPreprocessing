sendUpdateToConcole <- function(id, action = 'update', content = NULL, session = getDefaultReactiveDomain()) {
  messageJSON <- toJSON(list(
    'id' = id,
    'action' = action,
    'text' = content
  ), auto_unbox = TRUE)

  session$sendCustomMessage('rconsole', messageJSON)
}


withConsoleRedirect <- function(containerId, expr, logFile = NULL, warningFile = NULL) {
  suppressMessages(
    withCallingHandlers(
      results <- expr,
      message = function(m) {
        sendUpdateToConcole(id = containerId, content = m$message)
        if (!is.null(logFile)) write(m$message, logFile, append = TRUE)
      },
      warning = function(w) {
        parsedMessage <- paste0('Warning: ', w$message, '\n')
        sendUpdateToConcole(id = containerId, content = parsedMessage)
        if (!is.null(logFile)) write(parsedMessage, logFile, append = TRUE)
        if (!is.null(warningFile)) write(parsedMessage, warningFile, append = TRUE)
      }
    )
  )
  results
}


createLogFile <- function(outDir, type = 'log') {
  filePath <- file.path(outDir, paste0(type, '.txt'))
  write(paste0(toupper(type), ' file date: ', Sys.time(), '\n'), filePath)
  return(filePath)
}


coalesce_join <- function(x, y,
                          by = NULL, suffix = c(".x", ".y"),
                          join = dplyr::full_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))

  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce,
    1,
    nchar(to_coalesce) - nchar(suffix_used)
  ))

  suppressMessages(
    coalesced <- map_dfc(to_coalesce, ~coalesce(
      joined[[paste0(.x, suffix[1])]],
      joined[[paste0(.x, suffix[2])]]
    ))
  )
  names(coalesced) <- to_coalesce

  bind_cols(joined, coalesced) %>% select(all_of(cols))
}


warningMessage <- function(text) {
  message('\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
  warning(text, call. = FALSE, immediate. = TRUE)
  message('!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n')
}


