#' Read a saved database configuration
#'
#' @param key Name passed to [setupDatabaseKey()].
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
  config <- validateDatabaseConfig(config, checkCredentials = FALSE)

  # obscure password
  if (!showPassword) {
    pwdKeys <- c("pwd", "password")
    config <- newDatabaseConfig(config[!names(config) %in% pwdKeys])
  }

  return(config)
}

readConfig <- function(key) {
  kp <- keyPrefix(key)
  values <- as.list(Sys.getenv())
  values <- values[startsWith(names(values), kp)]
  names(values) <- sub(paste0("^", kp), "", names(values))
  names(values) <- tolower(names(values))
  newDatabaseConfig(values)
}

newDatabaseConfig <- function(config) {
  structure(.Data = config, class = c("database_config", "list"))
}

#' @export
print.database_config <- function(x, ...) {
  cat("<database_config>\n")
  if (!length(x)) {
    cat("  <empty>\n")
    return(invisible(x))
  }
  values <- unclass(x)
  secret <- names(values) %in% c("pwd", "password")
  values[secret] <- lapply(values[secret], function(x) "<hidden>")
  for (nm in names(values)) {
    value <- as.character(values[[nm]])
    cat("  ", nm, ": ", value, "\n", sep = "")
  }
  invisible(x)
}

validateDatabaseConfig <- function(config, checkCredentials = TRUE) {
  if (!is.list(config) || is.null(names(config))) {
    rlang::abort("`config` must be a named list.")
  }
  names(config) <- stringr::str_to_lower(names(config))
  if (anyDuplicated(names(config))) {
    rlang::abort("`config` cannot contain duplicate names.")
  }
  if (length(config$dbms) != 1L || !config$dbms %in% names(dbmsConfig)) {
    rlang::abort("`config$dbms` is not a supported database management system.")
  }
  required <- dbmsConfig[[config$dbms]]$required
  allowed <- unique(c(required, dbmsConfig[[config$dbms]]$connection))
  unknown <- setdiff(names(config), allowed)
  if (length(unknown)) {
    rlang::abort(paste0("Unknown configuration field(s): ", paste(unknown, collapse = ", "), "."))
  }
  missing <- required[purrr::map_lgl(required, function(x) {
    is.null(config[[x]]) || length(config[[x]]) != 1L ||
      is.na(config[[x]]) || !nzchar(as.character(config[[x]]))
  })]
  if (length(missing)) {
    rlang::abort(paste0("Missing required configuration field(s): ", paste(missing, collapse = ", "), "."))
  }
  if (isTRUE(checkCredentials)) validateConfigCredentials(config)
  structure(config, class = c("database_config", "list"))
}
