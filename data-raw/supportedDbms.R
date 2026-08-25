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
    "required" = c(commonFields, "port")
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
    )
  )
)

usethis::use_data(dbmsConfig, internal = TRUE, overwrite = TRUE)
