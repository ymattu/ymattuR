#' select columns without NA
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select_if
#' @param df Data Frame
select_nona_cols <- function(df) {
  res <- df %>%
    select_if(function(x) !any(is.na(x)))
  return(res)
}
