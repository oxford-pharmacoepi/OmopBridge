#' Save database credentials under a named key
#'
#' @param key Name used to retrieve the credentials later.
#' @param dbms Database management system
#' @param ... Named arguments to pass to the connection
#' @param cdmSchema Schema of the OMOP CDM tables.
#' @param resultsSchema Schema with writing permissions.
#' @param achillesSchema Schema that contains the achilles tables.
#' @param overwrite Whether to replace an existing configuration.
#'
#' @return Invisibly, the path to the updated environment file.
#'
#' @export
#'
setupDatabaseKey <- function(key,
                             dbms,
                             ...,
                             cdmSchema,
                             resultsSchema,
                             achillesSchema = resultsSchema,
                             overwrite = FALSE) {
  # input check
  omopgenerics::assertLogical(overwrite, length = 1)
  key <- validateKey(key, setup = TRUE, overwrite = overwrite)
  omopgenerics::assertChoice(dbms, names(dbmsConfig), length = 1)

}

