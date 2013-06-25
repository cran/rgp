\name{randfuncTyped}
\alias{randfuncTyped}
\alias{randfuncTypedRampedHalfAndHalf}
\title{Creates a well-typed R function with a random expression as its body}
\usage{
  randfuncTyped(type, funcset, inset, conset, maxdepth = 8,
    constprob = 0.2, exprfactory = randexprTypedGrow,
    breedingFitness = function(individual) TRUE,
    breedingTries = 50)

  randfuncTypedRampedHalfAndHalf(type, funcset, inset,
    conset, maxdepth = 8, constprob = 0.2,
    breedingFitness = function(individual) TRUE,
    breedingTries = 50)
}
\arguments{
  \item{type}{The range type of the random function to
  create.}

  \item{funcset}{The function set.}

  \item{inset}{The set of input variables.}

  \item{conset}{The set of constant factories.}

  \item{maxdepth}{The maximum expression tree depth.}

  \item{constprob}{The probability of generating a constant
  in a step of growth, if no subtree is generated. If
  neither a subtree nor a constant is generated, a randomly
  chosen input variable will be generated. Defaults to
  \code{0.2}.}

  \item{exprfactory}{The function to use for randomly
  creating the function's body.}

  \item{breedingFitness}{A breeding function. See the
  documentation for \code{\link{geneticProgramming}} for
  details.}

  \item{breedingTries}{The number of breeding steps.}
}
\value{
  A randomly generated well-typed R function.
}
\description{
  Creates a well-typed R function with a random expression
  as its body
}

