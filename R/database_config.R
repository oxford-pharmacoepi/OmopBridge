#' Read and validate a saved database configuration
#'
#' @param key Name passed to [setupDatabaseKey()].
#' @param showPassword Whether to include the password in the returned list.
#'
#' @return A named list of connection parameters.
#'
#' @export
#'
databaseConfig <- function(key,
                           showPassword = TRUE) {
  # input check
  key <- validateKey(key, setup = FALSE)

  prefix <- envPrefix(key)

  # fields
  fields <- as.list(Sys.getenv())
  fields <- fields[stringr::str_starts(names(fields), prefix)]
  names(fields) <- stringr::str_remove(string = names(fields), pattern = paste0("^", prefix))

  if (!showPassword) fields["pwd"] <- "<hidden>"

  fields
}

envPrefix <- function(key) {
  paste0("OMOPBRIDGE_", toupper(key), "_")
}
