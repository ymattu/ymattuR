#' Get The OS Type
#' @return OS Type (chr)
#' @export
get_os <- function() {
  if (.Platform$OS.type == "windows") {
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "mac"
  } else if (.Platform$OS.type == "unix") {
    "unix"
  } else {
    stop("Unknown OS")
  }
}

#' Whether We Can Connect to The Internet
#' @return T or F
#' @export
has_internet <- function() {
  ip <- get("nslookup", asNamespace("curl"))("r-project.org", error = FALSE)
  lgl <- NA
  if (is.null(ip)) {
    lgl <- FALSE
  } else {
    lgl <- TRUE
  }
  return(lgl)
}


#' Apply Multiple Functions to ONE Vector
#' @param x A Vector
#' @param ... Function Names
#' @return list
#' @examples
#' inverse_map(c(20, 100, 30), min, max)
#' @export
inverse_map <- function(x, ...){
  mthd <- list(...)
  lst <- lapply(mthd, function(m) do.call(m, list(x)))
  names(lst) <- as.character(mthd)
  return(lst)
}
