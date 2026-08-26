## code to configure different DBMS

commonFields <- c(
  "dbms",
  "server",
  "database",
  "uid",
  "pwd",
  "cdm_schema",
  "results_schema",
  "achilles_schema",
  "cdm_name"
)

dbmsConfig <- list(
  "postgres" = list(
    "pkgs" = c("DBI", "RPostgres", "CDMConnector"),
    "fun" = "DBI::dbConnect",
    "required" = c(commonFields, "port"),
    "connection" = c("server", "port", "database", "uid", "pwd", "sslmode"),
    "defaults" = list(port = 5432L)
  ),
  "sql server" = list(
    "pkgs" = c("DBI", "odbc", "CDMConnector"),
    "fun" = "DBI::dbConnect",
    "required" = c(
      commonFields,
      "driver",
      "cdm_catalog",
      "results_catalog",
      "achilles_catalog"
    ),
    "connection" = c("driver", "server", "port", "database", "uid", "pwd"),
    "defaults" = list(driver = "ODBC Driver 18 for SQL Server")
  )
)

usethis::use_data(dbmsConfig, internal = TRUE, overwrite = TRUE)
