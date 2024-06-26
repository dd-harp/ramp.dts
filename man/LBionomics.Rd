% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aquatic-interface.R
\name{LBionomics}
\alias{LBionomics}
\title{Set aquatic bionomic parameter rates relative to baseline}
\usage{
LBionomics(t, y, pars, s)
}
\arguments{
\item{t}{current simulation time}

\item{y}{state variables at time t}

\item{pars}{a \link{list}}

\item{s}{the species index}
}
\value{
a \link{list}
}
\description{
This method dispatches on the type of \code{pars$Lpar}. It should
compute the values of parameters as a function of exogenous variables
or reset the values of the bionomic parameters to baseline values.
}
