#' e_spatial
#'
#' Generates spatially structured error terms for appraisal simulation
#'
#' This function generates spatially structured error terms for appraisal simulation.
#' The spatial structure is provided by the spatial weight matrix parameter w, the
#' strength of the spatial influence by the parameter lambda. The function generates
#' ONE vector of spatially structured error terms from ONE vector of iid normal
#' inputs (parameter u).
#'
#' Parameters tst and nmax can be set to guide the number of iterations in the function.
#' Smaller values of tst lead to more precise results, but also longer computation.
#'
#' @param w A spatial weight matrix (a sparce matrix)
#' @param lambda The spatial weight parameter
#' @param u A vector of iid normal random terms
#' @param tst Criterion for stopping the iterations (default: 1E-7)
#' @param nmax maximum number of iterations (default: 50)
#'
#' @return A vector of spatially structured random variables
#' @export
#'
#' @examples
#' w <- listw2sparse(nm_listw)
#' eps <- rnorm(1953,0,1)
#' tst <- e_spatial(w, 0.5, eps, tst = 10^(-10))

e_spatial <- function(w, lambda, u, tst = 10^(-7), nmax = 50) {
  eps <- u
  n <- 0
  repeat {
    eps.neu <- lambda*w %*% eps + u
    eps2 <- (eps-eps.neu)*(eps-eps.neu)
    n <- n+1
    eps <- eps.neu
    m <- mean(eps2)
    if (m < tst) break
    if (n > nmax) stop("no convergence")
  }
  return(c(eps))
}
