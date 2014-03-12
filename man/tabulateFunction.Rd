\name{tabulateFunction}
\alias{tabulateFunction}
\title{Tabulate an n-ary function}
\usage{
tabulateFunction(f, ...)
}
\arguments{
  \item{f}{The function to tabulate.}

  \item{...}{For each dimension, a vector of sample points
  to calculate \code{f} at.}
}
\value{
A data frame of function values of \code{f}.
}
\description{
Creates a data frame of values for the n-ary function
\code{f} at the sample locations given in \code{...}.
}

