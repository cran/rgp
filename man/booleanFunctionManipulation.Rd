\name{integerToLogicals}
\alias{booleanFunctionAsList}
\alias{integerToLogicals}
\alias{makeBooleanFitnessFunction}
\alias{numberOfDifferentBits}
\title{Tools for manipulating boolean functions}
\usage{
integerToLogicals(i, width = floor(log(base = 2, i) + 1))

booleanFunctionAsList(f)

numberOfDifferentBits(a, b)

makeBooleanFitnessFunction(targetFunction)
}
\arguments{
  \item{i}{A scalar positive integer.}

  \item{width}{The with of the logical vector to return.}

  \item{f}{A boolean function.}

  \item{a}{A list of booleans.}

  \item{b}{A list of booleans.}

  \item{targetFunction}{A boolean function.}
}
\value{
The function result as described above.
}
\description{
\code{integerToBoolean} converts a scalar positive integer
(or zero) to its binary representation as list of logicals.
\code{booleanFunctionVector} returns the boolean vector of
result values of \code{f}, given a boolean function
\code{f}. \code{numberOfDifferentBits} given two lists of
booleans of equal length, returns the number of differing
bits. \code{makeBooleanFitnessFunction} given a boolean
target function, returns a fitness function that returns
the number of different places in the output of a given
boolean function and the target function.
}

