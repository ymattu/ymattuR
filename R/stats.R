#' build simple linear formula from variable names
#' @description build simple linear formula from variable names given by two
#'   character vectors. TODO: allow unquoted names.
#' @param left character vector
#' @param right character vector
#' @return formula
#' @importFrom stats as.formula
#' @examples
#' print(f <- build_formula(left = "A", right = c("B", "C")))
#' class(f)
#' build_formula(left = "Species", right = names(iris)[1:4])
#' @export
build_formula <- function(left, right) {
  stopifnot(is.character(left) && length(left) == 1)
  stopifnot(is.character(right) && length(right) > 0)
  as.formula(
    paste(
      paste(left, collapse = "+"),
      paste(right, collapse = "+"),
      sep = "~"
    )
  )
}
