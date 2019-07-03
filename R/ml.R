#' 信頼区間を算出する関数
#' @param sample 事後サンプルベクトル
#' @param interval 信頼区間の%
#' @importFrom stats quantile
#' @export
calc_ci <- function(sample, interval = 95) {
  quan <- (1 - (interval / 100)) / 2
  lower <- quantile(sample, quan)
  upper <- quantile(sample, 1 - quan)
  return(c(lower, upper))
}

#' RMSEを計算する関数
#' @param true 真値のベクトル(1個でも良い)
#' @param est 推定値のベクトル
#' @param n サンプルサイズ
#' @export
calc_rmse <- function(true, est, n = NULL) {
  if(!idntical(length(true), length(est))) {
    stop("Length of True and Est is different")
  }
  if(is.null(n)){
    n <- length(est)
  }
  res <- sqrt((sum((true - est) ^ 2, na.rm = T)) / n)
  return(res)
}

#' R2を計算する関数
#' @param true 真値のベクトル(1個でも良い)
#' @param est 推定値のベクトル
#' @param n サンプルサイズ
#' @export
calc_r2 <- function(true, est, n = NULL) {
  if(!identical(length(true), length(est))) {
    stop("Length of True and Est is different")
  }
  if(is.null(n)){
    n <- length(est)
  }
  res <- 1 - (sum((true - est) ^ 2, na.rm = T) / sum((true - mean(true)) ^ 2, na.rm = T))
  return(res)
}
