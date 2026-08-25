#' Create a CDM reference from a saved database key
#'
#' @param key Name passed to [setupDatabaseKey()].
#' @param writePrefix Prefix used for tables written by CDMConnector.
#' @return A CDM reference.
#' @export
cdmFromKey <- function(key, writePrefix = "omopbridge_") {
  omopgenerics::assertCharacter(writePrefix, length = 1)
  config <- databaseConfig(key, showPassword = TRUE)
  cdmFromConfig(config, writePrefix = writePrefix)
}

conFromConfig <- function(config) {
  config <- validateDatabaseConfig(config, checkCredentials = FALSE)
  if (config$dbms == "postgres") {
    connectionArgs <- list(
      drv = RPostgres::Postgres(),
      host = config$server,
      port = as.integer(config$port),
      dbname = config$database,
      user = config$uid,
      password = config$pwd
    )
    if (!is.null(config$sslmode)) connectionArgs$sslmode <- config$sslmode
    return(do.call(DBI::dbConnect, connectionArgs))
  }

  connectionArgs <- list(
    drv = odbc::odbc(),
    Driver = config$driver,
    Server = config$server,
    Database = config$database,
    UID = config$uid,
    PWD = config$pwd
  )
  if (!is.null(config$port)) connectionArgs$Port <- as.integer(config$port)
  do.call(DBI::dbConnect, connectionArgs)
}

cdmFromConfig <- function(config, writePrefix) {
  config <- validateDatabaseConfig(config, checkCredentials = FALSE)
  pkgs <- dbmsConfig[[config$dbms]]$pkgs
  rlang::check_installed(pkgs)
  con <- conFromConfig(config)

  if (config$dbms == "postgres") {
    return(CDMConnector::cdmFromCon(
      con = con,
      cdmSchema = config$cdm_schema,
      cdmName = config$cdm_name,
      writeSchema = config$results_schema,
      writePrefix = writePrefix,
      achillesSchema = config$achilles_schema
    ))
  }

  schema <- c(catalog = config$cdm_catalog, schema = config$cdm_schema)
  resultsSchema <- c(catalog = config$results_catalog, schema = config$results_schema)
  achillesSchema <- c(catalog = config$achilles_catalog, schema = config$achilles_schema)
  CDMConnector::cdmFromCon(
    con = con,
    cdmSchema = schema,
    cdmName = config$cdm_name,
    writeSchema = resultsSchema,
    writePrefix = writePrefix,
    achillesSchema = achillesSchema
  )
}
