% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_terms.R
\name{compute_NI}
\alias{compute_NI}
\title{Compute the NI}
\usage{
compute_NI(dts_out, pars, i)
}
\arguments{
\item{dts_out}{a matrix, the output of deSolve}

\item{pars}{a \link{list}}

\item{i}{the host species index}
}
\value{
\link{numeric} containing the NI
}
\description{
Using the output of \link[deSolve:ode]{deSolve::ode} or \link[deSolve:dede]{deSolve::dede},
compute the NI for each stratum
}
