withTestEnvironment <- function(code) {
  file <- tempfile(fileext = ".Renviron")
  oldOption <- getOption("omopbridge.env")
  oldNames <- names(Sys.getenv())
  options(omopbridge.env = file)
  on.exit({
    options(omopbridge.env = oldOption)
    currentNames <- names(Sys.getenv())
    newNames <- setdiff(currentNames, oldNames)
    newNames <- newNames[startsWith(newNames, "OMOPBRIDGE_")]
    if (length(newNames)) Sys.unsetenv(newNames)
    unlink(file)
  }, add = TRUE)
  force(code)
}
