#' Save a database configuration under a named key
#'
#' @param key Name used to retrieve the configuration later.
#' @param dbms Database management system. Currently `"postgres"` or
#'   `"sql server"`.
#' @param ... Database connection arguments. PostgreSQL accepts `server`,
#'   `port`, `database`, `uid`, `pwd`, and `sslmode`. SQL Server accepts
#'   `driver`, `server`, `port`, `database`, `uid`, and `pwd`.
#' @param cdmSchema Schema containing the OMOP CDM tables.
#' @param resultsSchema Schema where write tables are created.
#' @param achillesSchema Schema containing Achilles tables. Defaults to
#'   `resultsSchema`.
#' @param cdmCatalog Catalog containing the OMOP CDM tables. Required for
#'   SQL Server.
#' @param resultsCatalog Catalog where write tables are created. Required for
#'   SQL Server.
#' @param achillesCatalog Catalog containing Achilles tables. Required for
#'   SQL Server.
#' @param cdmName Name assigned to the CDM reference. Defaults to `key`.
#' @param overwrite Whether to replace an existing configuration.
#' @param check Whether to test the credentials before writing them.
#' @return Invisibly, `key`.
#' @export
setupDatabaseKey <- function(key,
                             dbms,
                             ...,
                             cdmSchema,
                             resultsSchema,
                             achillesSchema = resultsSchema,
                             cdmCatalog = NULL,
                             resultsCatalog = NULL,
                             achillesCatalog = NULL,
                             cdmName = NULL,
                             overwrite = FALSE,
                             check = TRUE) {
  omopgenerics::assertLogical(overwrite, length = 1)
  omopgenerics::assertLogical(check, length = 1)
  key <- validateKey(key, setup = TRUE, overwrite = overwrite)
  omopgenerics::assertChoice(dbms, names(dbmsConfig), length = 1)

  connectionConfig <- databaseConnectionConfig(dbms, list(...))
  schemaConfig <- databaseSchemaConfig(
    dbms = dbms,
    cdmSchema = cdmSchema,
    resultsSchema = resultsSchema,
    achillesSchema = achillesSchema,
    cdmCatalog = cdmCatalog,
    resultsCatalog = resultsCatalog,
    achillesCatalog = achillesCatalog
  )
  config <- c(
    list(dbms = dbms),
    connectionConfig,
    schemaConfig,
    list(cdm_name = if (is.null(cdmName)) key else cdmName)
  )
  config <- validateDatabaseConfig(config, checkCredentials = FALSE)

  if (check) {
    cli::cli_inform(c(i = "Checking that credentials are correct."))
    validateConfigCredentials(config)
    cli::cli_inform(c(v = "Credentials are good."))
  }

  writeKey(key, config)
  cli::cli_inform(c(v = "{.var {key}} written in the environment."))
  invisible(key)
}

databaseConnectionConfig <- function(dbms, config) {
  allowed <- dbmsConfig[[dbms]]$connection
  names(config) <- tolower(names(config))
  if (anyDuplicated(names(config))) {
    cli::cli_abort("Connection arguments cannot contain duplicate names.")
  }
  unknown <- setdiff(names(config), allowed)
  if (length(unknown)) {
    cli::cli_abort(c(
      x = "Unsupported connection argument{?s}: {unknown}.",
      i = "Supported arguments are: {allowed}."
    ))
  }
  if (is.null(config$pwd)) {
    if (!interactive()) cli::cli_abort("`pwd` must be supplied in a non-interactive session.")
    if (!requireNamespace("getPass", quietly = TRUE)) {
      cli::cli_abort("Package `getPass` is required to prompt for a password.")
    }
    config$pwd <- getPass::getPass(msg = "Database password: ")
  }
  defaults <- dbmsConfig[[dbms]]$defaults
  for (nm in names(defaults)) {
    if (is.null(config[[nm]])) config[[nm]] <- defaults[[nm]]
  }
  config
}

databaseSchemaConfig <- function(dbms, cdmSchema, resultsSchema, achillesSchema,
                                 cdmCatalog = NULL, resultsCatalog = NULL,
                                 achillesCatalog = NULL) {
  schemas <- list(
    cdm_schema = cdmSchema,
    results_schema = resultsSchema,
    achilles_schema = achillesSchema
  )
  for (i in seq_along(schemas)) {
    if (!is.character(schemas[[i]]) || length(schemas[[i]]) != 1L ||
        is.na(schemas[[i]]) || !nzchar(schemas[[i]])) {
      cli::cli_abort("All schema arguments must be one non-empty character value.")
    }
  }
  if (dbms == "postgres") {
    if (any(!vapply(list(cdmCatalog, resultsCatalog, achillesCatalog),
                    is.null, logical(1)))) {
      cli::cli_abort("Catalog arguments are only supported for SQL Server.")
    }
    return(schemas)
  }

  catalogs <- list(
    cdm_catalog = cdmCatalog,
    results_catalog = resultsCatalog,
    achilles_catalog = achillesCatalog
  )
  for (i in seq_along(catalogs)) {
    if (!is.character(catalogs[[i]]) || length(catalogs[[i]]) != 1L ||
        is.na(catalogs[[i]]) || !nzchar(catalogs[[i]])) {
      cli::cli_abort(paste0("`", names(catalogs)[[i]], "` must be one non-empty character value for SQL Server."))
    }
  }
  c(schemas, catalogs)
}

validateConfigCredentials <- function(config, call = parent.frame()) {
  writePrefix <- paste0(sample(letters, 3), collapse = "")
  cdm <- tryCatch(
    cdmFromConfig(config, writePrefix = paste0(writePrefix, "_")),
    error = function(e) e
  )
  if (inherits(cdm, "error")) {
    cli::cli_abort(
      c(x = "cdm_reference could not be created", `!` = conditionMessage(cdm)),
      call = call
    )
  }
  CDMConnector::cdmDisconnect(cdm)
  invisible()
}
