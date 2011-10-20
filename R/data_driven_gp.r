## data_driven_gp.r
##   - Data-driven Genetic Programming using the R formula interface
##
## RGP - a GP system for R
## 2010-2011 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' @include evolution.r
NA

##' Data-driven untyped standard genetic programming
##'
##' Perform an untyped genetic programming using a fitness function that depends
##' on a R data frame. Typical applications are data mining tasks such as symbolic
##' regression or classification. The task is specified as a \code{\link{formula}}
##' and a fitness function factory. Only simple formulas without interactions are
##' supported. The result of the data-driven GP run is a model structure containing
##' the formulas and an untyped GP population.
##' This function is primarily an intermediate for extensions. End-users will
##' probably use more specialized GP tools such as \code{\link{symbolicRegression}}.
##'
##' @param formula A \code{\link{formula}} describing the task. Only simple
##'   formulas of the form \code{response ~ variable1 + ... + variableN}
##'   are supported at this point in time.
##' @param data A \code{\link{data.frame}} containing training data for the
##'   GP run. The variables in \code{formula} must match column names in this
##'   data frame.
##' @param fitnessFunctionFactory A function that accepts two parameters, a
##'   code{\link{formula}}, data (given as a model frame) and the additional parameters
##'   given in \code{fitnessFunctionFactoryParameters} and returns a fitness function.
##' @param fitnessFunctionFactoryParameters Additional parameters to pass to the
##'   \code{fitnessFunctionFactory}.
##' @param stopCondition The stop condition for the evolution main loop. See
##'   \link{makeStepsStopCondition} for details.
##' @param population The GP population to start the run with. If this parameter
##'   is missing, a new GP population of size \code{populationSize} is created
##'   through random growth.
##' @param populationSize The number of individuals if a population is to be
##'   created.
##' @param eliteSize The number of elite individuals to keep. Defaults to
##'  \code{ceiling(0.1 * populationSize)}.
##' @param elite The elite list, must be alist of individuals sorted in ascending
##'   order by their first fitness component.
##' @param extinctionPrevention When set to \code{TRUE}, the initialization and
##'   selection steps will try to prevent duplicate individuals
##'   from occurring in the population. Defaults to \code{FALSE}, as this
##'   operation might be expensive with larger population sizes.
##' @param archive If set to \code{TRUE}, all GP individuals evaluated are stored in an
##'   archive list \code{archiveList} that is returned as part of the result of this function. 
##' @param genealogy If set to \code{TRUE}, the parent(s) of each indiviudal is stored in
##'   an archive \code{genealogyList} as part of the result of this function, enabling
##'   the reconstruction of the complete genealogy of the result population. This
##'   parameter implies \code{archive = TRUE}.
##' @param functionSet The function set.
##' @param constantSet The set of constant factory functions.
##' @param selectionFunction The selection function to use. Defaults to
##'   tournament selection. See \link{makeTournamentSelection} for details.
##' @param crossoverFunction The crossover function.
##' @param mutationFunction The mutation function.
##' @param restartCondition The restart condition for the evolution main loop. See
##'   \link{makeEmptyRestartCondition} for details.
##' @param restartStrategy The strategy for doing restarts. See
##'   \link{makeLocalRestartStrategy} for details.
##' @param breedingFitness A "breeding" function. This function is applied after
##'   every stochastic operation \emph{Op} that creates or modifies an individal
##'   (typically, \emph{Op} is a initialization, mutation, or crossover operation). If
##'   the breeding function returns \code{TRUE} on the given individual, \emph{Op} is
##'   considered a success. If the breeding function returns \code{FALSE}, \emph{Op}
##'   is retried a maximum of \code{breedingTries} times. If this maximum number of
##'   retries is exceeded, the result of the last try is considered as the result of
##'   \emph{Op}. In the case the breeding function returns a numeric value, the breeding
##'   is repeated \code{breedingTries} times and the individual with the lowest breeding
##'   fitness is considered the result of \emph{Op}.
##' @param breedingTries In case of a boolean \code{breedingFitness} function, the
##'   maximum number of retries. In case of a numerical \code{breedingFitness} function,
##'   the number of breeding steps. Also see the documentation for the \code{breedingFitness}
##'   parameter. Defaults to \code{50}.
##' @param progressMonitor A function of signature
##'   \code{function(population, fitnessfunction, stepNumber, evaluationNumber,
##'   bestFitness, timeElapsed)} to be called with each evolution step.
##' @param verbose Whether to print progress messages.
##' @return A model structure that contains the formula and an untyped GP population.
##'
##' @seealso \code{\link{geneticProgramming}}
##' @export
dataDrivenGeneticProgramming <- function(formula, data, fitnessFunctionFactory,
                                         fitnessFunctionFactoryParameters = list(),
                                         stopCondition = makeTimeStopCondition(5),
                                         population = NULL,
                                         populationSize = 100,
                                         eliteSize = ceiling(0.1 * populationSize),
                                         elite = list(),
                                         extinctionPrevention = FALSE,
                                         archive = FALSE,
                                         genealogy = FALSE,
                                         functionSet = mathFunctionSet,
                                         constantSet = numericConstantSet,
                                         selectionFunction = makeTournamentSelection(),
                                         crossoverFunction = crossover,
                                         mutationFunction = NULL,
                                         restartCondition = makeEmptyRestartCondition(),
                                         restartStrategy = makeLocalRestartStrategy(),
                                         breedingFitness = function(individual) TRUE,
                                         breedingTries = 50,
                                         progressMonitor = NULL,
                                         verbose = TRUE) {
  ## Match variables in formula to those in data or parent.frame() and
  ## return them in a new data frame. This also expands any '.'
  ## arguments in the formula.  
  mf <- model.frame(formula, data)
  ## Extract list of terms (rhs of ~) in expanded formula
  variableNames <- attr(terms(formula(mf)), "term.labels")
  ## Create inputVariableSet
  inVarSet <- inputVariableSet(list=as.list(variableNames))
  fitFunc <- do.call(fitnessFunctionFactory, c(list(formula(mf), mf), fitnessFunctionFactoryParameters))
  gpModel <- geneticProgramming(fitFunc,
                                stopCondition = stopCondition,
                                population = population,
                                populationSize = populationSize,
                                eliteSize = eliteSize,
                                elite = elite,
                                functionSet = functionSet,
                                inputVariables = inVarSet,
                                constantSet = constantSet,
                                selectionFunction = selectionFunction,
                                crossoverFunction = crossoverFunction,
                                mutationFunction = mutationFunction,
                                restartCondition = restartCondition,
                                restartStrategy = restartStrategy,
                                breedingFitness = breedingFitness,
                                breedingTries = breedingTries,
                                extinctionPrevention = extinctionPrevention,
                                archive = archive,
                                genealogy = genealogy,
                                progressMonitor = progressMonitor,
                                verbose = verbose)
  
  structure(append(gpModel, list(formula = formula(mf))),
                   class = c("dataDrivenGeneticProgrammingModel", "geneticProgrammingResult"))
}
