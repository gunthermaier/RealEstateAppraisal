#' model_quality
#'
#' A function to summarize indicators of model quality of a regression model
#'
#' @param reg A regression object
#'
#' @return Printed
#' @export
#'
#' @examples
#' model_quality(reg)

model_quality <- function (reg) {
  prnt.test <- function(x){
    cat(x, sep="\n")
  }
  ano <- anova(reg)                     ## store anova data frame
  k <- sum(ano$Df[1:length(ano$Df)-1])  ## store number of explanatory variables
  obs <- nobs(reg)                      ## store number of observations
  ## used in the regression
  ## compute sums of squares
  SST <- sum(ano$`Sum Sq`)
  SSM <- sum(ano$`Sum Sq`[1:length(ano$`Sum Sq`)-1])
  SSR <- ano$`Sum Sq`[length(ano$`Sum Sq`)]

  ## compute R-squared and adjusted R-squared
  Rsqu <- SSM/SST
  Rsqu.adj <- (Rsqu - k/(obs-1))*((obs-1)/(obs-k-1))

  ## compute mean squared errors
  MSEM = SSM/k
  MSER = SSR/ano$Df[length(ano$Df)]

  ## compute F statistic
  F <- MSEM/MSER

  ## Output all
  myvec <- c(sprintf("Total Sum of Squares: %e", SST),
             sprintf("Model Sum of Squares: %e", SSM),
             sprintf("Resid Sum of Squares: %e", SSR),
             sprintf("Number of observations: %i",obs),
             sprintf("Number of explanatory variables: %i",k),
             sprintf("R-squared: %f", Rsqu),
             sprintf("adj. R-squared: %f", Rsqu.adj),
             sprintf("Model MSE: %e", MSEM),
             sprintf("Resid MSE: %e", MSER),
             sprintf("F statistic: %f", F),
             sprintf("Residual standard error: %f", sqrt(MSER)))
  prnt.test(myvec)
}
