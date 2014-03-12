\name{joinElites}
\alias{joinElites}
\title{Join elite lists}
\usage{
joinElites(individuals, elite, eliteSize, fitnessFunction)
}
\arguments{
  \item{individuals}{The list of individuals to insert.}

  \item{elite}{The list of elite individuals to insert
  \code{individuals} into. This list must be sorted by
  fitness in ascending order, i.e. lower fitnesses first.}

  \item{eliteSize}{The maximum size of the \code{elite}.}

  \item{fitnessFunction}{The fitness function.}
}
\value{
The \code{elite} with \code{individuals} inserted, sorted
by fitness in ascending order, i.e. lower fitnesses first.
}
\description{
Inserts a list of new individuals into an elite list,
replacing the worst individuals in this list to make place,
if needed.
}

