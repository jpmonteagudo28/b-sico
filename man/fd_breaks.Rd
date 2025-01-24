% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/type_histogram.R
\name{fd_breaks}
\alias{fd_breaks}
\title{Freedman-Diaconis Rule for Number of Bins}
\usage{
fd_breaks(x)
}
\arguments{
\item{x}{A numeric vector representing the data to be binned.}
}
\value{
An integer representing the number of bins.
}
\description{
Computes the number of histogram bins using the Freedman-Diaconis rule, which is robust to outliers and uses the interquartile range (IQR) for bin width.
}
\details{
If the interquartile range (IQR) is zero, the function falls back to Scott's rule.
}
\examples{
x <- rnorm(100)
fd_breaks(x)

}
