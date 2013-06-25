\name{crossover}
\alias{crossover}
\alias{crossoverexpr}
\alias{crossoverexprFast}
\alias{crossoverexprTwoPoint}
\alias{crossoverexprTyped}
\alias{crossoverTyped}
\title{Random crossover (recombination) of functions and expressions}
\usage{
  crossover(func1, func2, crossoverprob = 0.1,
    breedingFitness = function(individual) TRUE,
    breedingTries = 50)

  crossoverexpr(expr1, expr2, crossoverprob)

  crossoverexprFast(expr1, expr2)

  crossoverexprTwoPoint(expr1, expr2)

  crossoverTyped(func1, func2, crossoverprob = 0.1,
    breedingFitness = function(individual) TRUE,
    breedingTries = 50)

  crossoverexprTyped(expr1, expr2, crossoverprob)
}
\arguments{
  \item{expr1}{The first parent R expression.}

  \item{func1}{The first parent R function.}

  \item{expr2}{The second parent R expression.}

  \item{func2}{The second parent R function.}

  \item{crossoverprob}{The probability of crossover at each
  node of the first parent function (expression).}

  \item{breedingFitness}{A breeding function. See the
  documentation for \code{\link{geneticProgramming}} for
  details.}

  \item{breedingTries}{The number of breeding steps.}
}
\value{
  The child function (expression) or functions
  (expressions).
}
\description{
  Replace a random subtree of \code{func1} (\code{expr1})
  with a random subtree of \code{func2} (\code{expr2}) and
  return the resulting function (expression), i.e. the
  modified \code{func1} (\code{expr1}).
  \code{crossoverexpr} handles crossover of expressions
  instead of functions. \code{crossoverexprFast} is a fast
  (i.e. implemented in efficient C code) albeit less
  flexible variant of \code{crossoverexpr}.
  \code{crossoverTyped} and \code{crossoverexprTyped} only
  exchage replace subtress if the sTypes of their root
  nodes match. \code{crossoverTwoPoint} is a variant of
  \code{crossover} that swaps subtrees at uniform randomly
  selected points and returns both children.
  \code{crossoverexprTwoPoint} works analogously for
  expressions.
}
\details{
  All RGP recombination operators operating on functions
  have the S3 class \code{c("recombinationOperator",
  "function")}.
}

