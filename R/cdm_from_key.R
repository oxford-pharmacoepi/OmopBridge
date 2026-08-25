#' Create a CDM reference from a saved database key
#' @param key Name passed to [setupDatabaseKey()].
#' @param ... Additional arguments passed to [CDMConnector::cdmFromCon()].
#' @return A CDM reference.
#' @export
cdmFromKey <- function(key, ...) {
  if (!requireNamespace("odbc", quietly = TRUE)) rlang::abort("Package 'odbc' is required.")
  if (!requireNamespace("CDMConnector", quietly = TRUE)) rlang::abort("Package 'CDMConnector' is required.")
  config <- databaseConfig(key)
  con <- DBI::dbConnect(odbc::odbc(), Driver = config$driver, Server = config$server,
                        Database = config$database, UID = config$uid, PWD = config$pwd)
  CDMConnector::cdmFromCon(con = con, ...)
}
