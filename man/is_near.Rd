% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_near.R
\name{is_near}
\alias{is_near}
\title{Compare two numeric vectors}
\usage{
is_near(x, y, tol = .Machine$double.eps^0.5)
}
\arguments{
\item{x, y}{Numeric vectors to compare}

\item{tol}{tolerance of comparison (optional)}
}
\value{
a logical array (same size as input arrays) with
  element-wise comparison of `x` and `y``
}
\description{
This is a safe way of comparing if two vectors of floating
point numbers are (pairwise) equal. This is safer than
using `==``, because it has a built in tolerance. This function
is based on `dplyr::near()`.
}
\examples{
sqrt(2)^2 == 2
is_near(sqrt(2)^2, 2)
}
