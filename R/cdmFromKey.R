#' Create a CDM reference from a saved database key
#'
#' @param key Name passed to [setupDatabaseKey()].
#' @param writePrefix Write prefix.
#'
#' @return A CDM reference.
#'
#' @export
#'
cdmFromKey <- function(key,
                       writePrefix = paste0(paste0(sample(letters, 3), collapse = ""), "_")) {
}

conFromConfig <- function(config) {
  if (config$dbms == "postgres") {
    DBI::dbConnect(
      drv = RPostgres::Postgres(),
    )
  } else if (config$dbms == "sql server") {
    DBI::dbConnect(
      drv = odbc::odbc(),
    )
  }
}

cdmFromConfig <- function(config, writePrefix) {
  dbms <- config$dbms
  pkgs <- dbmsConfig[[dbms]]$pkgs

  rlang::check_installed(pkg = pkgs)

  con <- conFromConfig(config)

  if (config$dbms == "postgres") {
    CDMConnector::cdmFromCon(
      con = con,
      cdmSchema = config$cdm_schema,
      cdmName = config$cdm_name,
      writeSchema = config$cdm_schema,
      writePrefix = writePrefix,
      achillesSchema = config$achilles_schema
    )
  } else if (config$dbms == "sql server") {
    CDMConnector::cdmFromCon(
      con = con,
      cdmSchema = c(catalog = config$cdm_catalog, schema = config$cdm_schema),
      cdmName = config$cdm_name,
      writeSchema = c(catalog = config$cdm_catalog, schema = config$cdm_schema),
      writePrefix = writePrefix,
      achillesSchema = c(catalog = config$achilles_catalog, schema = config$achilles_schema)
    )
  }

}
