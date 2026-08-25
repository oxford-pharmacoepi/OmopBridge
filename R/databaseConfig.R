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
  # input check
  key <- validateKey(key, setup = FALSE)
  omopgenerics::assertLogical(showPassword, length = 1)

  config <- readConfig(key)

  # obscure password
  if (!showPassword) {
    pwdKeys <- c("pwd", "password")
    config <- config[!names(config) %in% pwdKeys]
  }

  return(config)
}

readConfig <- function(key) {
  kp <- keyPrefix(key)
  values <- as.list(Sys.getenv())
  values <- values[startsWith(names(values), kp)]
  names(values) <- stringr::str_remove(names(values), paste0("^", kp))
  names(values) <- stringr::str_to_lower(names(values))
  newDatabaseConfig(values)
}

newDatabaseConfig <- function(config) {
  structure(.Data = config, class = c("database_config", "list"))
}

#' @export
print.database_config <- function(x, ...) {

}
