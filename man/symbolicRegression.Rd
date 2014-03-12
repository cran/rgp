\name{symbolicRegression}
\alias{symbolicRegression}
\title{Symbolic regression via untyped standard genetic programming}
\usage{
symbolicRegression(formula, data, stopCondition = makeTimeStopCondition(5),
  population = NULL, populationSize = 100, eliteSize = ceiling(0.1 *
  populationSize), elite = list(), extinctionPrevention = FALSE,
  archive = FALSE, individualSizeLimit = 64,
  penalizeGenotypeConstantIndividuals = FALSE, subSamplingShare = 1,
  functionSet = mathFunctionSet, constantSet = numericConstantSet,
  crossoverFunction = NULL, mutationFunction = NULL,
  restartCondition = makeEmptyRestartCondition(),
  restartStrategy = makeLocalRestartStrategy(),
  searchHeuristic = makeAgeFitnessComplexityParetoGpSearchHeuristic(),
  breedingFitness = function(individual) TRUE, breedingTries = 50,
  errorMeasure = rmse, progressMonitor = NULL, envir = parent.frame(),
  verbose = TRUE)
}
\arguments{
  \item{formula}{A \code{\link{formula}} describing the
  regression task. Only simple formulas of the form
  \code{response ~ variable1 + ... + variableN} are
  supported at this point in time.}

  \item{data}{A \code{\link{data.frame}} containing
  training data for the symbolic regression run. The
  variables in \code{formula} must match column names in
  this data frame.}

  \item{stopCondition}{The stop condition for the evolution
  main loop. See \link{makeStepsStopCondition} for
  details.}

  \item{population}{The GP population to start the run
  with. If this parameter is missing, a new GP population
  of size \code{populationSize} is created through random
  growth.}

  \item{populationSize}{The number of individuals if a
  population is to be created.}

  \item{eliteSize}{The number of elite individuals to keep.
  Defaults to \code{ceiling(0.1 * populationSize)}.}

  \item{elite}{The elite list, must be alist of individuals
  sorted in ascending order by their first fitness
  component.}

  \item{extinctionPrevention}{When set to \code{TRUE}, the
  initialization and selection steps will try to prevent
  duplicate individuals from occurring in the population.
  Defaults to \code{FALSE}, as this operation might be
  expensive with larger population sizes.}

  \item{archive}{If set to \code{TRUE}, all GP individuals
  evaluated are stored in an archive list
  \code{archiveList} that is returned as part of the result
  of this function.}

  \item{individualSizeLimit}{Individuals with a number of
  tree nodes that exceeds this size limit will get a
  fitness of \code{Inf}.}

  \item{penalizeGenotypeConstantIndividuals}{Individuals
  that do not contain any input variables will get a
  fitness of \code{Inf}.}

  \item{subSamplingShare}{The share of fitness cases
  \deqn{s} sampled for evaluation with each function
  evaluation. \deqn{0 < s \leq 1} must hold, defaults to
  \code{1.0}.}

  \item{functionSet}{The function set.}

  \item{constantSet}{The set of constant factory
  functions.}

  \item{crossoverFunction}{The crossover function.}

  \item{mutationFunction}{The mutation function.}

  \item{restartCondition}{The restart condition for the
  evolution main loop. See \link{makeEmptyRestartCondition}
  for details.}

  \item{restartStrategy}{The strategy for doing restarts.
  See \link{makeLocalRestartStrategy} for details.}

  \item{searchHeuristic}{The search-heuristic (i.e.
  optimization algorithm) to use in the search of
  solutions. See the documentation for
  \code{searchHeuristics} for available algorithms.}

  \item{breedingFitness}{A "breeding" function. This
  function is applied after every stochastic operation
  \emph{Op} that creates or modifies an individal
  (typically, \emph{Op} is a initialization, mutation, or
  crossover operation). If the breeding function returns
  \code{TRUE} on the given individual, \emph{Op} is
  considered a success. If the breeding function returns
  \code{FALSE}, \emph{Op} is retried a maximum of
  \code{breedingTries} times. If this maximum number of
  retries is exceeded, the result of the last try is
  considered as the result of \emph{Op}. In the case the
  breeding function returns a numeric value, the breeding
  is repeated \code{breedingTries} times and the individual
  with the lowest breeding fitness is considered the result
  of \emph{Op}.}

  \item{breedingTries}{In case of a boolean
  \code{breedingFitness} function, the maximum number of
  retries. In case of a numerical \code{breedingFitness}
  function, the number of breeding steps. Also see the
  documentation for the \code{breedingFitness} parameter.
  Defaults to \code{50}.}

  \item{errorMeasure}{A function to use as an error
  measure, defaults to RMSE.}

  \item{progressMonitor}{A function of signature
  \code{function(population, fitnessValues,
  fitnessFunction, stepNumber, evaluationNumber,
  bestFitness, timeElapsed)} to be called with each
  evolution step.}

  \item{envir}{The R environment to evaluate individuals
  in, defaults to \code{parent.frame()}.}

  \item{verbose}{Whether to print progress messages.}
}
\value{
An symbolic regression model that contains an untyped GP
population.
}
\description{
Perform symbolic regression via untyped genetic
programming. The regression task is specified as a
\code{\link{formula}}. Only simple formulas without
interactions are supported. The result of the symbolic
regression run is a symbolic regression model containing an
untyped GP population of model functions.
}
\seealso{
\code{\link{predict.symbolicRegressionModel}},
\code{\link{geneticProgramming}}
}

