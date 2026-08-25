#' Save database credentials under a named key
#'
#' @param key Name used to retrieve the credentials later.
#' @param dbms Database management system
#' @param ... Named arguments to pass to the connection
#' @param cdmSchema Schema of the OMOP CDM tables.
#' @param resultsSchema Schema with writing permissions.
#' @param achillesSchema Schema that contains the achilles tables.
#' @param envFile File to update. Defaults to the user's `~/.Renviron`.
#' @param overwrite Whether to replace an existing configuration.
#'
#' @return Invisibly, the path to the updated environment file.
#'
#' @export
#'
setupDatabaseKey <- function(key,
                             dbms,
                             ...,
                             cdmSchema,
                             resultsSchema,
                             achillesSchema = resultsSchema,
                             envFile = path.expand("~/.Renviron"),
                             overwrite = FALSE) {
  # input check
  omopgenerics::assertLogical(overwrite, length = 1)
  key <- validateKey(key, setup = TRUE, overwrite = overwrite)
  omopgenerics::assertChoice(dbms, names(dbmsConfig), length = 1)

}

validateKey <- function(key, setup, overwrite, call = parent.frame()) {
  omopgenerics::assertCharacter(key, length = 1, call = call)

  ak <- availableKeys()

  if (setup) {
    if (key %in% ak) {
      if (overwrite) {
        # Delete the existing key from the current R session. Its entries in
        # the .Renviron file are replaced below when the new values are saved.
        fields <- c("DRIVER", "SERVER", "DATABASE", "UID", "PWD")
        Sys.unsetenv(paste0(envPrefix(key), fields))
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

availableKeys <- function() {
  envNames <- names(Sys.getenv())
  prefix <- "^OMOPBRIDGE_([A-Z][A-Z0-9_]*)_DBMS$"
  keys <- sub(prefix, "\\1", envNames[grepl(prefix, envNames)])
  keys <- unique(keys)
  tolower(keys)
}
