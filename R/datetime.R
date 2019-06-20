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
