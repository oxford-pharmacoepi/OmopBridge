postgresConfig <- function(...) {
  config <- list(
    dbms = "postgres",
    server = "localhost",
    port = 5432,
    database = "omop",
    uid = "user",
    pwd = "secret",
    cdm_schema = "cdm",
    results_schema = "results",
    achilles_schema = "achilles",
    cdm_name = "test"
  )
  overrides <- list(...)
  config[names(overrides)] <- overrides
  config
}

test_that("valid configurations are given the database_config class", {
  config <- OmopBridge:::validateDatabaseConfig(postgresConfig(), checkCredentials = FALSE)
  expect_s3_class(config, "database_config")
})

test_that("unknown and missing configuration fields are rejected", {
  expect_error(
    OmopBridge:::validateDatabaseConfig(postgresConfig(port = NULL), checkCredentials = FALSE),
    "port"
  )
  expect_error(
    OmopBridge:::validateDatabaseConfig(postgresConfig(unexpected = "value"), checkCredentials = FALSE),
    "unexpected"
  )
})

test_that("connection arguments are mapped for PostgreSQL", {
  config <- postgresConfig(sslmode = "require")
  arguments <- OmopBridge:::connectionArguments(config, drv = "postgres-driver")
  expect_equal(arguments$drv, "postgres-driver")
  expect_equal(arguments$host, "localhost")
  expect_equal(arguments$port, 5432L)
  expect_equal(arguments$dbname, "omop")
  expect_equal(arguments$user, "user")
  expect_equal(arguments$password, "secret")
  expect_equal(arguments$sslmode, "require")
})

test_that("connection arguments are mapped for SQL Server", {
  config <- list(
    dbms = "sql server",
    driver = "ODBC Driver 18 for SQL Server",
    server = "sql-server",
    port = 1433,
    database = "database",
    uid = "user",
    pwd = "secret",
    cdm_schema = "cdm",
    results_schema = "results",
    achilles_schema = "achilles",
    cdm_catalog = "omop",
    results_catalog = "results",
    achilles_catalog = "results",
    cdm_name = "test"
  )
  arguments <- OmopBridge:::connectionArguments(config, drv = "odbc-driver")
  expect_equal(arguments$drv, "odbc-driver")
  expect_equal(arguments$Driver, config$driver)
  expect_equal(arguments$Server, config$server)
  expect_equal(arguments$Port, 1433L)
  expect_equal(arguments$Database, config$database)
  expect_equal(arguments$UID, config$uid)
  expect_equal(arguments$PWD, config$pwd)
})
