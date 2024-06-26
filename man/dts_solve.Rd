% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dts_solve.R
\name{dts_solve}
\alias{dts_solve}
\title{Solve a system of equations}
\usage{
dts_solve(pars, Tmax = 365)
}
\arguments{
\item{pars}{a \link{list} that defines a model}

\item{Tmax}{the last time point, run from 0...Tmax}
}
\value{
a \link{list}
}
\description{
This method dispatches on the type of \code{pars$dts}.
}
