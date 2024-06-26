% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adult-lumpy.R
\name{F_fqZ.RMlumpy}
\alias{F_fqZ.RMlumpy}
\title{The net blood feeding rate of the infective mosquito population in a patch}
\usage{
\method{F_fqZ}{RMlumpy}(t, y, pars, s)
}
\arguments{
\item{t}{current simulation time}

\item{y}{state vector}

\item{pars}{a \link{list}}

\item{s}{the species index}
}
\value{
a \link{numeric} vector of length \code{nPatches}
}
\description{
Implements \link{F_fqZ} for the RMlumpy model.
}
