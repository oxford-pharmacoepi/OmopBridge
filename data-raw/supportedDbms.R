## code to configure different DBMS

dbmsConfig <- list(
  "postgres" = list(
    "pkgs" = c("DBI", "RPostgres", "CDMConnector"),
    "fun" = "DBI::dbConnect"
  ),
  "sql server" = list(
    "pkgs" = c("DBI", "odbc", "CDMConnector"),
    "fun" = "DBI::dbConnect"
  )
)

usethis::use_data(dbmsConfig, internal = TRUE, overwrite = TRUE)
