#' Read a saved database configuration
#'
#' @param key Name passed to [addKey()].
#' @param showPassword Whether to include the password in the returned list.
#'
#' @return A named list of configuration values. The password is hidden by
#'   default.
#'
#' @export
#'
databaseConfig <- function(key,
                           showPassword = FALSE) {
  if (!is.logical(showPassword) || length(showPassword) != 1L || is.na(showPassword)) {
    rlang::abort("showPassword must be a single TRUE or FALSE value.")
  }
  key <- validateKey(key, setup = FALSE)

  prefix <- keyPrefix(key)

  fields <- Sys.getenv()
  fields <- fields[startsWith(names(fields), prefix)]
  names(fields) <- tolower(sub(paste0("^", prefix), "", names(fields)))
  fields <- as.list(fields)
  if (!showPassword && "pwd" %in% names(fields)) fields$pwd <- "<hidden>"
  fields
}

newDatabaseConfig <- function(config) {
  structure(.Data = config, class = c("database_config", "list"))
}

#' @export
print.database_config <- function(x, ...) {

}
