#' appraisalsummary
#'
#' Outputs the results of the appraisal generated by the appraisal functions
#'
#' @param myin A list with the result generated by one of the appraisal functions
#' @param simu A parameter that is FALSE (the appraisal is derived from the distribution) or TRUE (the appraisal is derived by simulation)
#'
#' @return nothing -- the function prints
#' @export
#'
#' @examples
#' appraisalsummary(myin)
appraisalsummary <- function(myin, simu=FALSE) {
  mynames <- names(myin$input)
  mylogp <- c("Expected log(p):          %10.2f  %10.2f  %10.2f",
              "Variance log(p):          %10.5f  %10.5f  %10.5f",
              "Std.Dev. log(p):          %10.5f  %10.5f  %10.5f")
  myp    <- c("Expected price:           %.4e  %.4e  %.4e",
              "Variance of price:        %.4e  %.4e  %.4e",
              "Std.Dev. of price:        %.4e  %.4e  %.4e",
              "Conf.Int. Lower:          %10.1f  %10.1f  %10.1f",
              "Conf.Int. Upper:          %10.1f  %10.1f  %10.1f",
              "Conf.Int. Range:          %10.1f  %10.1f  %10.1f")
  mylogv <- c("Expected log(v):          %10.2f  %10.2f  %10.2f",
              "Variance log(v):          %.4e  %.4e  %.4e",
              "Std.Dev. log(v):          %10.5f  %10.5f  %10.5f")
  myv    <- c("Expected value:           %.4e  %.4e  %.4e",
              "Variance of value:        %.4e  %.4e  %.4e",
              "Std.Dev. of value:        %.4e  %.4e  %.4e",
              "Conf.Int. Lower:          %10.1f  %10.1f  %10.1f",
              "Conf.Int. Upper:          %10.1f  %10.1f  %10.1f",
              "Conf.Int. Range:          %10.1f  %10.1f  %10.1f")
  if(nrow(myin$output) == 18){
    myformat <- c(mylogp, myp, mylogv, myv)
    mybreak <- 9
    myheader <- "Appraisal based on loglinear regression"
    if (simu) {
      myheader <- paste(myheader,"--- simulated")
    }
  } else {
    myformat <- c(myp, myv)
    mybreak <- 6
    myheader <- "Appraisal based on linear regression"
  }


  prnt.test <- function(x){
    cat(x, sep="\n")
  }
  ## Input
  in_df <- myin$input
  myvec = c(myheader, "", sprintf("%36s  %10s  %10s", paste("Object",mynames[2]), paste("Object",mynames[3]), paste("Object",mynames[4])))
  for (i in 1:nrow(in_df)) {
    myvec <- c(myvec, sprintf("%-30s %5.2f       %5.2f       %5.2f", in_df[i,1], in_df[i,2], in_df[i,3], in_df[i,4]))
  }
  prnt.test(myvec)

  ## Output
  o_df = myin$output
  myvec = c("","========","Price Forecast")
  for (i in 1:nrow(o_df)) {
    myvec <- c(myvec, sprintf(myformat[i], o_df[i,1], o_df[i,2], o_df[i,3]))
    if (i == mybreak) { myvec <- c(myvec, "--------", "Market Value Forecast")}
  }
  myvec <- c(myvec,"========")
  prnt.test(myvec)
}
