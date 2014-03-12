\name{makeFunctionFitnessFunction}
\alias{makeFunctionFitnessFunction}
\title{Create a fitness function from a reference function of one variable}
\usage{
makeFunctionFitnessFunction(func, from = -1, to = 1, steps = 128,
  errorMeasure = rmse, indsizelimit = NA)
}
\arguments{
  \item{func}{The reference function.}

  \item{from}{The start of the sequence of fitness cases.}

  \item{to}{The end of the sequence of fitness cases.}

  \item{steps}{The number of steps in the sequence of
  fitness cases.}

  \item{errorMeasure}{A function to use as an error
  measure, defaults to RMSE.}

  \item{indsizelimit}{Individuals exceeding this size limit
  will get a fitness of \code{Inf}.}
}
\value{
A fitness function based on the reference function
\code{func}.
}
\description{
Creates a fitness function that calculates an error measure
with respect to an arbitrary reference function of one
variable on the sequence of fitness cases \code{seq(from,
to, length = steps)}. When an \code{indsizelimit} is given,
individuals exceeding this limit will receive a fitness of
\code{Inf}.
}

