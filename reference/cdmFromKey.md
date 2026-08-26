# Create a CDM reference from a saved database key

Create a CDM reference from a saved database key

## Usage

``` r
cdmFromKey(key, writePrefix = "omopbridge_")
```

## Arguments

- key:

  Name passed to
  [`setupDatabaseKey()`](https://oxford-pharmacoepi.github.io/OmopBridge/reference/setupDatabaseKey.md).

- writePrefix:

  Prefix used for tables written by CDMConnector.

## Value

A CDM reference.
