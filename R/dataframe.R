#' select columns without NA
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select_if
#' @param df Data Frame
#' @export
select_nona_cols <- function(df) {
  res <- df %>%
    select_if(function(x) !any(is.na(x)))
  return(res)
}

#' Delete all 0 columns
#'
#' @param df data Frame
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @export
del_all0_cols <- function(df) {
  delcols <- df %>%
    select_if(is.numeric) %>%
    colSums() %>%
    .[. == 0] %>%
    names()
  res <- df %>%
    select(-delcols)
  return(res)
}
