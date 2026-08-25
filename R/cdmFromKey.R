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
  dbms <- config$dbms
  pkgs <- dbmsConfig[[dbms]]$pkgs

  rlang::check_installed(pkg = pkgs)

  if (dbms == "postgres") {
    DBI::dbConnect(
      drv = RPostgres::Postgres(),
    )
  } else if (dbms == "sql server") {
    DBI::dbConnect(
      drv = odbc::odbc(),
    )
  }
}
