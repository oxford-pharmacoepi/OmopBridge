# Save a database configuration under a named key

Save a database configuration under a named key

## Usage

``` r
setupDatabaseKey(
  key,
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
  check = TRUE
)
```

## Arguments

- key:

  Name used to retrieve the configuration later.

- dbms:

  Database management system. Currently `"postgres"` or `"sql server"`.

- ...:

  Database connection arguments. PostgreSQL accepts `server`, `port`,
  `database`, `uid`, `pwd`, and `sslmode`. SQL Server accepts `driver`,
  `server`, `port`, `database`, `uid`, and `pwd`.

- cdmSchema:

  Schema containing the OMOP CDM tables.

- resultsSchema:

  Schema where write tables are created.

- achillesSchema:

  Schema containing Achilles tables. Defaults to `resultsSchema`.

- cdmCatalog:

  Catalog containing the OMOP CDM tables. Required for SQL Server.

- resultsCatalog:

  Catalog where write tables are created. Required for SQL Server.

- achillesCatalog:

  Catalog containing Achilles tables. Required for SQL Server.

- cdmName:

  Name assigned to the CDM reference. Defaults to `key`.

- overwrite:

  Whether to replace an existing configuration.

- check:

  Whether to test the credentials before writing them.

## Value

Invisibly, `key`.
