\name{seSymbolic}
\alias{seSymbolic}
\title{Symbolic squared error (SE)}
\usage{
  seSymbolic(f, g, lower, upper, subdivisions = 100)
}
\arguments{
  \item{f}{An R function.}

  \item{g}{An R function with the same formal arguments as
  \code{f}.}

  \item{lower}{The lower limit of integraion.}

  \item{upper}{The upper limit of integraion.}

  \item{subdivisions}{The maximum number of subintervals
  for numeric integration.}
}
\value{
  The area of the squared differences between \code{f} and
  \code{g}, or \code{Inf} if integration is not possible in
  the limits given.
}
\description{
  Given to functions \code{f} and \code{g}, returns the
  area the squared differences between \code{f} and
  \code{g} in the integration limits \code{lower} and
  \code{upper}.
}

