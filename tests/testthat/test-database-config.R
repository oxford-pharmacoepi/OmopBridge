test_that("PostgreSQL configurations can be created and read", {
  withTestEnvironment({
    setupDatabaseKey(
      key = "test_postgres",
      dbms = "postgres",
      server = "localhost",
      database = "omop",
      uid = "user",
      pwd = "secret",
      cdmSchema = "cdm",
      resultsSchema = "results",
      check = FALSE
    )

    config <- databaseConfig("test_postgres")
    expect_s3_class(config, "database_config")
    expect_equal(config$dbms, "postgres")
    expect_equal(config$port, "5432")
    expect_null(config$pwd)

    config <- databaseConfig("test_postgres", showPassword = TRUE)
    expect_equal(config$pwd, "secret")
  })
})

test_that("SQL Server configurations require and preserve catalogs", {
  withTestEnvironment({
    setupDatabaseKey(
      key = "test_sql_server",
      dbms = "sql server",
      server = "sql-server",
      database = "connection_database",
      uid = "user",
      pwd = "secret",
      cdmSchema = "cdm",
      cdmCatalog = "omop",
      resultsSchema = "results",
      resultsCatalog = "results",
      achillesSchema = "achilles",
      achillesCatalog = "results",
      check = FALSE
    )

    config <- databaseConfig("test_sql_server", showPassword = TRUE)
    expect_equal(config$driver, "ODBC Driver 18 for SQL Server")
    expect_equal(config$cdm_catalog, "omop")
    expect_equal(config$results_catalog, "results")
    expect_equal(config$achilles_catalog, "results")
  })
})

test_that("SQL Server setup rejects missing catalogs", {
  withTestEnvironment({
    expect_error(
      setupDatabaseKey(
        key = "invalid_sql_server",
        dbms = "sql server",
        server = "sql-server",
        database = "database",
        uid = "user",
        pwd = "secret",
        cdmSchema = "cdm",
        resultsSchema = "results",
        check = FALSE
      ),
      "cdm_catalog"
    )
  })
})

test_that("overwriting a key replaces its configuration", {
  withTestEnvironment({
    setupDatabaseKey(
      key = "overwrite_key",
      dbms = "postgres",
      server = "old-server",
      database = "omop",
      uid = "user",
      pwd = "secret",
      cdmSchema = "cdm",
      resultsSchema = "results",
      check = FALSE
    )
    setupDatabaseKey(
      key = "overwrite_key",
      dbms = "postgres",
      server = "new-server",
      database = "omop",
      uid = "user",
      pwd = "secret",
      cdmSchema = "cdm",
      resultsSchema = "results",
      overwrite = TRUE,
      check = FALSE
    )

    config <- databaseConfig("overwrite_key", showPassword = TRUE)
    expect_equal(config$server, "new-server")
    expect_length(grep("old-server", readLines(OmopBridge:::envFile(), warn = FALSE)), 0)
  })
})

test_that("database configuration printing hides secrets", {
  config <- structure(
    list(dbms = "postgres", pwd = "secret", server = "localhost"),
    class = c("database_config", "list")
  )
  output <- capture.output(print(config))
  expect_true(any(grepl("<hidden>", output, fixed = TRUE)))
  expect_false(any(grepl("secret", output, fixed = TRUE)))
})

test_that("removing a key clears an active session value even without a file", {
  withTestEnvironment({
    key <- "orphan_key"
    variable <- paste0("OMOPBRIDGE_", toupper(key), "_DBMS")
    do.call(Sys.setenv, as.list(setNames("postgres", variable)))

    OmopBridge:::removeKey(key)

    expect_false(nzchar(Sys.getenv(variable, unset = "")))
  })
})
