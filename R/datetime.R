#' Unixtime to Datetime JST
#' @param x int
#' @importFrom lubridate as_datetime
#' @export
unixtime2jst <- function(x) {
  if(!is.integer(x)) {
    x <- as.integer(x)
  }
  res <- as_datetime(x, tz="Asia/Tokyo")
  return(res)
}

#' Calculate Age Fast
#'
#' @param from character string (or vector), like '1994-10-17'
#' @param to character string (or vector), like '2019-06-22'
#' @importFrom dplyr if_else
#' @export
age <- function(from, to) {
  from_lt <- as.POSIXlt(from)
  to_lt <- as.POSIXlt(to)

  age <- to_lt$year - from_lt$year

  if_else(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1L, age)
}
