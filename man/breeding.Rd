\name{breed}
\alias{breed}
\title{Breeding of GP individuals}
\usage{
  breed(individualFactory, breedingFitness, breedingTries,
    warnOnFailure = TRUE, stopOnFailure = FALSE)
}
\arguments{
  \item{individualFactory}{A function of no parameters that
  returns a single GP individual.}

  \item{breedingFitness}{Either a function that takes a GP
  individual as its only parameter and returns a single
  logical value or a function that takes a GP individual as
  its only parameter and returns a single real value.}

  \item{breedingTries}{The number of breeding steps to
  perform. In case of a boolean \code{breedingFitness}
  function, the actual number of breeding steps performed
  may be lower then this number (see the details).}

  \item{warnOnFailure}{Whether to issue a warning when a
  boolean \code{breedingFitness} predicate was not
  fulfilled after \code{breedingTries} tries.}

  \item{stopOnFailure}{Whether to stop with an error
  message when a boolean \code{breedingFitness} predicate
  was not fulfilled after \code{breedingTries} tries.}
}
\value{
  The GP individual that was bred.
}
\description{
  Breeds GP individuals by repeated application of an
  individual factory function. \code{individualFactory}.
  The \code{breedingFitness} must be a function of domain
  logical (a single boolean value) or numeric (a single
  real number). In case of a boolean breeding function,
  candidate individuals are created via the
  \code{individualFactory} function and tested by the
  \code{breedingFitness} predicate until the
  \code{breedingFitness} predicate is \code{TRUE} or
  \code{breedingTries} tries were done, in which case the
  last individual created and tested is returned. In case
  of a numerical breeding function, \code{breedingTries}
  individuals are created and evaluated by the
  \code{breedingFitness} function. The individual with the
  minimal breeding fitness is returned.
}

