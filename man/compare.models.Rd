% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare.models.R
\name{compare.models}
\alias{compare.models}
\title{compare.models}
\usage{
compare.models(formula, data, w, type = "lag")
}
\arguments{
\item{formula}{A lm-type regression formula}

\item{data}{The data-frame}

\item{w}{A spatial weight matrix in listw format}

\item{type}{either "lag" or "err"}
}
\value{
prints the comparison table
}
\description{
Compare non-spatial and spatial models
}
\examples{
compare.models(formula, hedonicdata_no_na, nm_listw, "err")
compare.models(formula, hedonicdata_no_na, nm_listw, "lag")
}
