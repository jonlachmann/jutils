% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quantiles.R
\name{rowQuantmean}
\alias{rowQuantmean}
\title{Calculate the quantiles and mean of rows in a matrix}
\usage{
rowQuantmean(mat, quantiles = c(0.05, 0.95), mean = TRUE)
}
\arguments{
\item{mat}{The matrix to use}

\item{quantiles}{The quantiles to calculate, defaults to 0.05 and 0.95 (i.e. 90\%)}

\item{mean}{Should the mean also be calculated? Defaults to TRUE}
}
\value{
A list containing the mean of each row, together with the low and high quantiles selected
}
\description{
Calculate the quantiles and mean of rows in a matrix
}
