\name{safeDivide}
\alias{ifPositive}
\alias{ifThenElse}
\alias{ln}
\alias{positive}
\alias{safeDivide}
\alias{safeLn}
\alias{safeSqroot}
\title{Some simple arithmetic and logic functions for use in GP expressions}
\usage{
safeDivide(a, b)

safeSqroot(a)

safeLn(a)

ln(a)

positive(x)

ifPositive(x, thenbranch, elsebranch)

ifThenElse(x, thenbranch, elsebranch)
}
\arguments{
  \item{a}{A numeric value.}

  \item{b}{A numeric value.}

  \item{x}{A numeric value.}

  \item{thenbranch}{The element to return when \code{x} is
  \code{TRUE}.}

  \item{elsebranch}{The element to return when \code{x} is
  \code{FALSE}.}
}
\description{
\code{safeDivide} a division operator that returns 0 if the
divisor is 0. \code{safeLn} a natural logarithm operator
that return 0 if its argument is less then 0. \code{ln} is
the natural logarithm. \code{positive} returns true if its
argument is greater then 0. \code{ifPositive} returns its
second argument if its first argument is positive,
otherwise its third argument. \code{ifThenElse} returns its
second argument if its first argument is \code{TRUE},
otherwise its third argument.
}

