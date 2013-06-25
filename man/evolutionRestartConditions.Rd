\name{makeEmptyRestartCondition}
\alias{makeEmptyRestartCondition}
\alias{makeFitnessDistributionRestartCondition}
\alias{makeFitnessStagnationRestartCondition}
\alias{makeStepLimitRestartCondition}
\title{Evolution restart conditions}
\usage{
  makeEmptyRestartCondition()

  makeStepLimitRestartCondition(stepLimit = 10)

  makeFitnessStagnationRestartCondition(fitnessHistorySize = 100,
    testFrequency = 10,
    fitnessStandardDeviationLimit = 1e-06)

  makeFitnessDistributionRestartCondition(testFrequency = 100,
    fitnessStandardDeviationLimit = 1e-06)
}
\arguments{
  \item{stepLimit}{The step limit for
  \code{makeStepLimitRestartCondition}.}

  \item{fitnessHistorySize}{The number of past best fitness
  values to look at when calculating the best fitness
  standard deviation for
  \code{makeFitnessStagnationRestartCondition}.}

  \item{testFrequency}{The frequency to test for the
  restart condition, in evolution steps. This parameter is
  mainly used with restart condititions that are expensive
  to calculate.}

  \item{fitnessStandardDeviationLimit}{The best fitness
  standard deviation limit for
  \code{makeFitnessStagnationRestartCondition}.}
}
\description{
  Evolution restart conditions are predicates (functions
  that return a single logical value) of the signature
  \code{function(population, fitnessFunction, stepNumber,
  evaluationNumber, bestFitness, timeElapsed)}. They are
  used to decide when to restart a GP evolution run that
  might be stuck in a local optimum. Evolution restart
  conditions are objects of the same type and class as
  evolution stop conditions. They may be freely substituted
  for each other.
}
\details{
  \code{makeEmptyRestartCondition} creates a restart
  condition that is never fulfilled, i.e. restarts will
  never occur. \code{makeStepLimitRestartCondition} creates
  a restart condition that holds if the number if evolution
  steps is an integer multiple of a given step limit.
  restarts will never occur.
  \code{makeFitnessStagnationRestartCondition} creates a
  restart strategy that holds if the standard deviation of
  a last \code{fitnessHistorySize} best fitness values
  falls below a given \code{fitnessStandardDeviationLimit}.
  \code{makeFitnessDistributionRestartCondition} creates a
  restart strategy that holds if the standard deviation of
  the fitness values of the individuals in the current
  population falls below a given
  \code{fitnessStandardDeviationLimit}.
}

