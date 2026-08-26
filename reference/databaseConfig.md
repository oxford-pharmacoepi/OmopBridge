# Read a saved database configuration

Read a saved database configuration

## Usage

``` r
databaseConfig(key, showPassword = FALSE)
```

## Arguments

- key:

  Name passed to
  [`setupDatabaseKey()`](https://oxford-pharmacoepi.github.io/OmopBridge/reference/setupDatabaseKey.md).

- showPassword:

  Whether to include the password in the returned list.

## Value

A named list of configuration values. The password is hidden by default.
