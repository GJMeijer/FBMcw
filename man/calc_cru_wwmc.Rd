% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_cru_wwmc.R
\name{calc_cru_wwmc}
\alias{calc_cru_wwmc}
\title{Calculate peak root reinforcement according to WWMc model}
\usage{
calc_cru_wwmc(drmin, drmax, tru0, betat, phir0, betaphi, k = 1.2, dr0 = 1)
}
\arguments{
\item{drmin, drmax}{minumum and maximum root diameter in bundle
(numeric array)}

\item{tru0}{Tensile strength of root with reference diameter (numeric array)}

\item{betat}{Power law coefficient for root diameter-root tensile strength
fit (numeric array)}

\item{phir0}{Root area ratio of root with reference diameter (numeric array)}

\item{betaphi}{Power law coefficient for root diameter-root area ratio
fit (numeric array)}

\item{k}{Wu/Waldron factor accounting for root orientations at failure
(numeric scalar, default 1.2)}

\item{dr0}{Reference diameter (numeric scalar, default 1.0)}
}
\value{
numeric array with peak reinforcement preductions `cru_wwmc`
}
\description{
This function calculates the peak root reinforcement according to the
Wu/Waldron model, assuming a continuous, power-law distribution of root area ratio
across a range of root diameters
}
\examples{
calc_cru_wwmc(1, 5, 10, -0.5, 0.01, -0.2)
}
