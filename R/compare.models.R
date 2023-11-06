#' compare.models
#'
#' Compare non-spatial and spatial models
#'
#' @param formula A lm-type regression formula
#' @param data The data-frame
#' @param w A spatial weight matrix in listw format
#' @param type either "lag" or "err"
#'
#' @return prints the comparison table
#' @export
#'
#' @examples
#' compare.models(formula, hedonicdata_no_na, nm_listw, "err")
#' compare.models(formula, hedonicdata_no_na, nm_listw, "lag")

compare.models <- function(formula, data, w, type = "lag") {
  reg <- lm(formula, data=data)
  reg.coef <- reg$coefficients
  reg.stderr <- sqrt(diag(vcov(reg)))
  reg.t <- reg.coef/reg.stderr
  if (type == "lag") {
    lag_model <- lagsarlm(formula, data = data, w)
    lag.coef <- lag_model$coefficients
    lag.stderr <- sqrt(diag(vcov(lag_model)))[-1]
    lag.t <- lag.coef/lag.stderr
    ratio.coef <- lag.coef/reg.coef
    ratio.t <- lag.t/reg.t
    info <- cbind(reg.coef, lag.coef, reg.t, lag.t, ratio.coef, ratio.t)
  } else if (type == "err") {
    err_model <- errorsarlm(formula, data = data, w)
    err.coef <- err_model$coefficients
    err.stderr <- sqrt(diag(vcov(err_model)))[-1]
    err.t <- err.coef/err.stderr
    ratio.coef <- err.coef/reg.coef
    ratio.t <- err.t/reg.t
    info <- cbind(reg.coef, err.coef, reg.t, err.t, ratio.coef, ratio.t)
  }
  print(info)
}
