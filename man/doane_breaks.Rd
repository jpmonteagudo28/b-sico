% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/type_histogram.R
\name{doane_breaks}
\alias{doane_breaks}
\title{Doane's Formula for Number of Bins}
\usage{
doane_breaks(x)
}
\arguments{
\item{x}{A numeric vector representing the data to be binned.}
}
\value{
An integer representing the number of bins.
}
\description{
Computes the number of histogram bins using Doane's formula, which adjusts Sturges' rule to account for skewness in the data.
}
\examples{
x <- rnorm(100)
doane_breaks(x)

}
