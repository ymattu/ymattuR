#' Get The Path of Current File
#' @importFrom magrittr %>%
#' @importFrom stringr str_which str_subset str_replace
#' @importFrom rstudioapi isAvailable getActiveDocumentContext getSourceEditorContext
#' @importFrom here here
#' @export
whoAmI <- function() {
  # http://stackoverflow.com/a/32016824/2292993
  res <- NA
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  needle <- "--file="
  matches <- str_which(cmdArgs, needle)
  if (length(matches) > 0) {
    # Rscript via command line
    res <- cmdArgs %>%
      str_subset(., needle) %>%
      str_replace(., needle, "") %>%
      normalizePath()
  } else if (isAvailable()) {
    # on RStudio
    res <- getSourceEditorContext()$path %>%
      normalizePath()
  } else {
    print(here())
  }
  return(res)
}

#' Get The Directory Where Current File Is Saved
#' @export
whereAmI <- function() {
  return(dirname(whoAmI()))
}
