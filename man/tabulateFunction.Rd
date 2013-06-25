\name{tabulateFunction}
\alias{tabulateFunction}
\title{Tabulate a function}
\usage{
  tabulateFunction(f, ...)
}
\arguments{
  \item{f}{The function to tabulate.}

  \item{...}{The points to tabulate \code{f} at.}
}
\value{
  A data frame of function values of \code{f}.
}
\description{
  Creates a data frame of values for the function \code{f}
  at the domain points given in \code{...}. This function
  works like \code{\link{mapply}}, but returns a data frame
  that also contains the domain points instead of a simple
  vector that only contains (range) values of \code{f}.
  When using element names in \code{...}, these names must
  match the formal parameter names of \code{f}.
}
\seealso{
  \code{\link{mapply}}
}

