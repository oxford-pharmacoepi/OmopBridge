envFile <- function() {
  getOption("omopbridge.env", default = path.expand("~/.Renviron"))
}

prefix <- function() {
  getOption("omopbridge.prefix", default = "OMOPBRIDGE_")
}

keyPrefix <- function(key) {
  paste0(prefix(), toupper(key), "_")
}

availableKeys <- function() {
  envNames <- names(Sys.getenv())
  value <- paste0("^", prefix(), "(.*)_DBMS$")
  keys <- sub(value, "\\1", envNames[grepl(value, envNames)])
  tolower(sort(unique(keys)))
}

validateKey <- function(key, setup, overwrite, call = parent.frame()) {
  omopgenerics::assertCharacter(key, length = 1, call = call)

  ak <- availableKeys()

  if (setup) {
    if (key %in% ak) {
      if (overwrite) {
        return(key)
      } else {
        cli::cli_abort(c(x = "`key` already exists and overwrite is FALSE."))
      }
    } else {
      return(key)
    }
  } else {
    if (key %in% ak) {
      return(key)
    } else {
      cli::cli_abort(c(x = "`key` does not exist!"))
    }
  }
}

writeKey <- function(key, config) {
  removeKey(key)

  file <- envFile()
  old <- readLines(file, warn = FALSE)
  keyPrefixValue <- keyPrefix(key)

  namesToWrite <- paste0(keyPrefixValue, toupper(names(config)))
  newLines <- paste0(namesToWrite, "=", vapply(config, envQuote, character(1)))
  writeLines(c(old, newLines), file)

  names(config) <- namesToWrite
  do.call(Sys.setenv, config)

  invisible(file)
}

removeKey <- function(key) {
  file <- envFile()
  keyPrefixValue <- keyPrefix(key)

  if (!file.exists(file)) {
    writeLines(character(), con = file)
    return(invisible(file))
  }

  old <- readLines(file, warn = FALSE)
  isKeyLine <- startsWith(trimws(old), keyPrefixValue)
  writeLines(old[!isKeyLine], file)

  sessionNames <- names(Sys.getenv())
  sessionKeyNames <- sessionNames[startsWith(sessionNames, keyPrefixValue)]
  if (length(sessionKeyNames) > 0) {
    Sys.unsetenv(sessionKeyNames)
  }

  invisible(file)
}

envQuote <- function(x) {
  paste0("'", gsub("'", "'\"'\"'", as.character(x), fixed = TRUE), "'")
}
