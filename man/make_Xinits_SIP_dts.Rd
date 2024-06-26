% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/human-SIP-dts.R
\name{make_Xinits_SIP_dts}
\alias{make_Xinits_SIP_dts}
\title{Make initial values for the SIP_dts human model, with defaults}
\usage{
make_Xinits_SIP_dts(
  nStrata,
  Xopts = list(),
  H0 = NULL,
  S0 = NULL,
  I0 = 1,
  P0 = 0
)
}
\arguments{
\item{nStrata}{the number of population strata}

\item{Xopts}{a \link{list} that could overwrite defaults}

\item{H0}{the initial human population density}

\item{S0}{the initial values of the parameter S}

\item{I0}{the initial values of the parameter I}

\item{P0}{the initial values of the parameter P}
}
\value{
a \link{list}
}
\description{
Make initial values for the SIP_dts human model, with defaults
}
