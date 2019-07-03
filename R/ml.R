#' Calculate Confidential Inteval
#'
#' @param sample 事後サンプルベクトル
#' @param interval 信頼区間の\%
#' @importFrom stats quantile
#' @export
calc_ci <- function(sample, interval = 95) {
  quan <- (1 - (interval / 100)) / 2
  lower <- quantile(sample, quan)
  upper <- quantile(sample, 1 - quan)
  return(c(lower, upper))
}

#' Calculate RMSE
#' @param true 真値のベクトル(1個でも良い)
#' @param est 推定値のベクトル
#' @param n サンプルサイズ
#' @export
calc_rmse <- function(true, est, n = NULL) {
  if(!identical(length(true), length(est))) {
    stop("Length of True and Est is different")
  }
  if(is.null(n)){
    n <- length(est)
  }
  res <- sqrt((sum((true - est) ^ 2, na.rm = T)) / n)
  return(res)
}

#' Calculate R2
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

#' Calculate AUC and Plot ROC Curve
#' @param actual 真値のベクトル
#' @param score 予測値のベクトル
#' @importFrom ggplot2 ggplot geom_path scale_x_continuous scale_y_continuous theme ggtitle geom_text
#' @importFrom scales percent
#' @export
plot_roc <- function(actual, score) {
  ### ROC-AUC計算
  o <- order(score, decreasing = T)
  fp <- tp <- fp_prev <- tp_prev <- 0
  nF <- sum(actual == F)
  nT <- sum(actual == T)
  score_prev <- -Inf
  ber_min <- Inf
  area <- 0
  rx <- ry <- numeric(length(o))
  n <- 0

  for (i in seq_along(o)) {
    j <- o[i]
    if (score[j] != score_prev) {
      area <- area + (fp - fp_prev) * (tp + tp_prev) / 2
      n <- n + 1
      rx[n] <- fp/nF
      ry[n] <- tp/nT
      ber <- (fp/nF + 1 - tp/nT)/2
      AUC <- area/(nF*nT)

      if (ber < ber_min) {
        ber_min <- ber
        th <- score_prev
        rx_best <- fp/nF
        ry_best <- tp/nT
      }
      score_prev <- score[j]
      fp_prev <- fp
      tp_prev <- tp
    }
    if (actual[j] == T) {
      tp <- tp + 1
    } else {
      fp <- fp + 1
    }
  }
  area <- area + (fp - fp_prev) * (tp + tp_prev) / 2
  AUC <- area/(nF*nT)
  n <- n + 1
  rx[n] <- fp/nF  # = 1
  ry[n] <- tp/nT  # = 1

  ###ggplot2
  AUC <- round(AUC, digits = 4)
  tmp <- data.frame(x = rx, y = ry)
  label <- paste("AUC = ", AUC)
  gg <- ggplot(tmp) +
    geom_path(aes(x, y, color = "red"), size = 1.0) +
    geom_path(data = data.frame(x = c(0,1), y = c(0, 1)), aes(x, y),
              colour = "gray", size = 1.0) +
    scale_x_continuous("False_Positive Rate (1 - Specificity)",
                       labels = percent, limits = c(0, 1)) +
    scale_y_continuous("True_Positive Rate (Sensivity or Recall)",
                       labels = percent, limits = c(0, 1)) +
    theme(legend.position = "none") +
    ggtitle("ROC Curve") +
    geom_text(data = NULL, x = 0.85, y = 0.15, label = label, colour = "black")
  print(gg)

  ###ROC-AUC計算
  cat("AUC =", area/(nF*nT), "th =", th, "\n")
  cat("BER =", (rx_best + (1-ry_best))/2,
      "OR =", (ry_best/(1-ry_best))/(rx_best/(1-rx_best)), "\n")
}
