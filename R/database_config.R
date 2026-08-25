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
  fields <- c("DRIVER", "SERVER", "DATABASE", "UID", "PWD")
  values <- Sys.getenv(paste0(envPrefix(key), fields), unset = "")
  names(values) <- tolower(fields)
  missing_fields <- names(values)[!nzchar(values)]
  if (length(missing_fields)) {
    rlang::abort(paste0("Missing configuration for '", key, "': ", paste(missing_fields, collapse = ", "), "."))
  }
  if (!showPassword) values["pwd"] <- "<hidden>"
  as.list(values)
}

envPrefix <- function(key) {
  paste0("OMOPBRIDGE_", toupper(key), "_")
}
