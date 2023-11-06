#' w_powers
#'
#' Generates powers of the spatial weight matrix (for approximate matrix inverse)
#'
#' This function generates the powers of the spatial weight matrix (parameter w)
#' as input for the function w_inv that calculates the approximate inverse. The function
#' returns the values for the last row (the property to appraise) of the respective
#' matrices.
#'
#' @param w A spatial weight matrix (A sparce matrix)
#' @param max The number of powers to calculate
#'
#' @return A matrix with max+1 rows and the same number of columns as w
#' @export
#'
#' @examples
#' m <- w_powers(w)

w_powers <- function(w, max = 5){
  l1 <- length(w[1,])
  I <- diag(l1)
  result <- I[l1,]
  result <- c(result,w[l1,])
  n <- 2
  w1 <- w
  repeat{
    if (n > max) break
    w1 <- w1 %*% w
    result <- c(result,w1[l1,])
    n <- n+1
  }
  m <- matrix(result, ncol=l1, byrow=TRUE)
  return(m)
}
