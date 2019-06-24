#' \code{ymattuR} package
#'
#' Utilities for y__mattu
#'
#' See the README on
#' \href{https://github.com/ymattu/ymattuR#readme}{GitHub}
#'
#' @docType package
#' @name ymattuR
NULL

#' @useDynLib ymattuR, .registration=TRUE
#' @importFrom Rcpp sourceCpp
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", ".data"))
