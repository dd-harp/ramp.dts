% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adult-lumpy.R
\name{list_MYZvars.RMlumpy}
\alias{list_MYZvars.RMlumpy}
\title{Return the variables as a list}
\usage{
\method{list_MYZvars}{RMlumpy}(y, pars, s)
}
\arguments{
\item{y}{the variables}

\item{pars}{a \link{list}}

\item{s}{the vector species index}
}
\value{
a \link{list}
}
\description{
This method dispatches on the type of \code{pars$MYZpar[[s]]}
}
