#' Save database credentials under a named key
#'
#' @param key Name used to retrieve the credentials later.
#' @param dbms Database management system
#' @param ... Named arguments to pass to the connection
#' @param cdmSchema Schema of the OMOP CDM tables.
#' @param resultsSchema Schema with writing permissions.
#' @param achillesSchema Schema that contains the achilles tables.
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
                             cdmName = NULL,
                             overwrite = FALSE,
                             check = TRUE) {
  # input check
  omopgenerics::assertLogical(overwrite, length = 1)
  key <- validateKey(key, setup = TRUE, overwrite = overwrite)
  omopgenerics::assertChoice(dbms, names(dbmsConfig), length = 1)
  omopgenerics::assertLogical(check, length = 1)

  # create config object

  if (check) {
    cli::cli_inform(c(i = "Checking that credentials are correct."))
    # check that cdm can be created from config information
    validateConfigCredentials(config)
    cli::cli_inform(c(v = "Credentials are good."))
  }

  # write config
  writeKey(key, config)
  cli::cli_inform(c(v = "{.var {key}} written in the environment."))

  invisible(key)
}

validateConfigCredentials <- function(config, call = parent.frame()) {
  wp <- sample(letters, 3) |>
    paste0(collapse = "") |>
    paste0("_")

  cdm <- tryCatch({
    cdmFromConfig(config, writePrefix = wp)
  }, error = function(e) as.character(e))
  if (is.character(cdm)) {
    cli::cli_abort(c(x = "cdm_reference could not be created", "!" = cdm), call = call)
  }

  invisible()
}
