#' w_inv
#'
#' Calculates the approximate inverse of the matrix (I-lambda*w).
#'
#' The function uses the output of the function w_powers and a simulated value for
#' lambda to calculate the approximate inverse of the matrix (I-lambda*w).
#'
#' @param m The matrix with the last row of the powers of the spatial weight matrix
#' @param lambda A value for the spatial weight parameter
#'
#' @return The approximate inverse of the matrix (I-lambda*w).
#' @export
#'
#' @examples
#' tst <- w_powers(w, max=5)
#' inv <- w_inv(tst, 0.5)

w_inv <- function(m, lambda){
  l1 <- length(m[,1])-1
  result <- 1
  for (i in 1:l1){
    result <- c(result, lambda^i)
  }
  l <- matrix(result, nrow = 1, byrow = TRUE)
  rw <- l%*%m
  return(rw)
}
