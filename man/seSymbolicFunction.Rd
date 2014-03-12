\name{seSymbolicFunction}
\alias{seSymbolicFunction}
\title{Symbolic squared error function (SE)}
\usage{
seSymbolicFunction(f, g)
}
\arguments{
  \item{f}{An R function.}

  \item{g}{An R function with the same formal arguments as
  \code{f}.}
}
\value{
A function representing the squared error between \code{f}
and \code{g}.
}
\description{
Given two functions \code{f} and \code{g}, returns a
function whose body is the symbolic representation of the
squared error between \code{f} and \code{g}, i.e.
\code{function(x) (f(x) - g(x))^2}.
}

