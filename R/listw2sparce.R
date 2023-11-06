#' listw2sparce
#'
#' Converts a listw object (spatial weight matrix) to a sparce matrix.
#'
#' The function checks the size of the input listw object. If it contains
#' more than 50,000 elements, the function aborts and issues a notification.
#' This is typically the case with distance band based spatial weight matrices.
#' To force the function to run anyways, set the parameter force to TRUE.
#'
#' @param nm_listw A listw object
#' @param force A logical variable that forces computation (default: FALSE)
#'
#' @return A sparce matrix
#' @export
#'
#' @examples
#' w <- listw2sparse(nm_listw)
listw2sparse <- function(nm_listw, force=FALSE) {
  n <- length(nm_listw$neighbours)
  fields = 0
  for (i in (1:n)) {
    myrow <- nm_listw$neighbours[[i]]
    nr <- length(myrow)
    fields <- fields + nr
  }
  if ((fields > 50000)&(!force)) {
    print (paste("Too many elements in input list: ", fields))
    print ("It may take a while to run the function.")
    print ("Set 'force=TRUE' to run anyways.")
    return(NULL)
  }
  w <- Matrix(data=0, nrow=n, ncol=n, sparse=TRUE)
  for (i in (1:n)) {
    myrow <- nm_listw$neighbours[[i]]
    nr <- length(myrow)
    for (j in (1:nr)) {
      wj = myrow[j]
      w[i,wj] <- nm_listw$weights[[i]][j]
    }
  }
  return(w)
}
