\name{makeSeSymbolicFitnessFunction}
\alias{makeSeSymbolicFitnessFunction}
\title{Create a fitness function based on symbolic squared error (SE)}
\usage{
makeSeSymbolicFitnessFunction(func, lower, upper, subdivisions = 100,
  indsizelimit = NA)
}
\arguments{
  \item{func}{The reference function.}

  \item{lower}{The lower limit of integraion.}

  \item{upper}{The upper limit of integraion.}

  \item{subdivisions}{The maximum number of subintervals
  for numeric integration.}

  \item{indsizelimit}{Individuals exceeding this size limit
  will get a fitness of \code{Inf}.}
}
\value{
A fitness function based on the reference function
\code{func}.
}
\description{
Creates a fitness function that calculates the squared
error of an individual with respect to a reference function
\code{func}. When an \code{indsizelimit} is given,
individuals exceeding this limit will receive a fitness of
\code{Inf}.
}

