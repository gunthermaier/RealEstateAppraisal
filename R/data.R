#' Demo dataset, real property in Vienna, Austria
#'
#' Sales prices and characteristics of real property in Vienna, Austria
#'
#' This dataset is used for demonstration purposes in the book
#'
#'    "Real Estate Appraisal with Hedonic Price Models"
#'         by Gunther Maier and Shanaka Herath
#'
#' The data contains information about sales prices and characteristics
#' of appartments in Vienna Austria.
#' The dataset was provided by ERESnet GmbH in Vienna, Austria
#'
#' @format ## `hedonicdata`
#' A data frame with 4,105 rows and 69 columns. Not all variables are used
#' in the analysis. The most important variables are:
#' \describe{
#'   \item{price}{Sales price of the appartment}
#'   \item{plz}{ZIP-code; corresponds largely to city districts}
#'   \item{distancecbd}{Distance to the city center}
#'   \item{latitude}{Latitude of the appartment location}
#'   \item{longitude}{Longitude of the appartment location}
#'   ...
#' }
#' @source {ERESnet GmbH, Vienna, Austria}
#' @examples data(hedonicdata)
"hedonicdata"
