## evolution.r
##   - Functions defining typical evolution main loops,
##     some typical GP function- and constant sets
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' @include search_space.r
NA

##' Genetic programming run
##'
##' Perform a genetic programming run. The required argument \code{fitnessFunction}
##' must be supplied with an objective function that assigns a numerical fitness
##' value to an R function. Fitness values are minimized, i.e. smaller values mean
##' higher/better fitness. If a multi-objective \code{selectionFunction} is
##' used, \code{fitnessFunction} must be a list of objective functions.
##' The result of the genetic programming run is a genetic programming model
##' containing a GP population of R functions.
##'
##' @param fitnessFunction In case of a single-objective selection function,
##'   \code{fitnessFunction} must be a single function that assigns a
##'   numerical fitness value to a GP individual represented as a R function.
##'   Smaller fitness values mean higher/better fitness. If a multi-objective
##'   selection function is used, \code{fitnessFunction} must be a list of
##'   of objective functions.
##' @param stopCondition The stop condition for the evolution main loop. See
##'   \link{makeStepsStopCondition} for details.
##' @param population The GP population to start the run with. If this parameter
##'   is missing, a new GP population of size \code{populationSize} is created
##'   through random growth.
##' @param populationSize The number of individuals if a population is to be
##'   created.
##' @param functionSet The function set.
##' @param inputVariables The input variable set.
##' @param constantSet The set of constant factory functions.
##' @param selectionFunction The selection function to use. Defaults to
##'   \code{tournamentSelection}. See \link{tournamentSelection} for details.
##' @param crossoverFunction The crossover function.
##' @param mutationFunction The mutation function.
##' @param progressMonitor A function of signature
##'   \code{function(population, stepNumber, timeElapsed)} to be called
##'   with each evolution step.
##' @param verbose Whether to print progress messages.
##' @return A genetic programming model that contains a GP population in the
##'   field \code{population}, as well as metadata describing the run parameters.
##'
##' @export
geneticProgramming <- function(fitnessFunction,
                               stopCondition = makeTimeStopCondition(5),
                               population = NULL,
                               populationSize = 500,
                               functionSet = mathFunctionSet,
                               inputVariables = inputVariableSet("x"),
                               constantSet = numericConstantSet,
                               selectionFunction = tournamentSelection,
                               crossoverFunction = crossover,
                               mutationFunction = NULL,
                               progressMonitor = NULL,
                               verbose = TRUE) {
  logmsg <- function(msg, ...) {
    if (verbose)
      message(sprintf(msg, ...))
  }
  
  progmon <-
    if (is.null(progressMonitor) && verbose) {
      function(pop, stepNumber, timeElapsed)
        if (stepNumber %% 100 == 0)
          logmsg("evolution step %i, time elapsed: %f seconds", stepNumber, round(timeElapsed, 2))
    } else if (is.null(progressMonitor)) {
      function(pop, stepNumber, timeElapsed) NULL # verbose == FALSE, do not show progress
    } else
      progressMonitor
  mutatefunc <-
    if (is.null(mutationFunction)) {
      function(ind) mutateSubtree(mutateNumericConst(ind),
                                  functionSet, inputVariables, constantSet, mutatesubtreeprob = 0.01)
    } else
      mutationFunction
  
  pop <-
    if (is.null(population))
      makePopulation(populationSize, functionSet, inputVariables, constantSet)
    else
      population
  stepNumber <- 1
  startTime <- proc.time()["elapsed"]
  timeElapsed <- 0

  logmsg("STARTING genetic programming evolution run...")
  while (!stopCondition(pop = pop, stepNumber = stepNumber, timeElapsed = timeElapsed)) {
    selA <- selectionFunction(pop, fitnessFunction)
    selB <- selectionFunction(pop, fitnessFunction)
    winnerA <- selA$selectedIndex
    winnerB <- selB$selectedIndex
    losersA <- selA$fitnessValues[!(selA$fitnessValues[, 1] == selA$selectedIndex), 1]
    losersB <- selA$fitnessValues[!(selA$fitnessValues[, 1] == selA$selectedIndex), 1]
    losers <- c(losersA, losersB)
    pop[losers] <-
      replicate(length(losers), mutatefunc(crossoverFunction(pop[[winnerA]], pop[[winnerB]])))
    
    timeElapsed <- proc.time()["elapsed"] - startTime
    stepNumber <- 1 + stepNumber
    progmon(pop = pop, stepNumber = stepNumber, timeElapsed = timeElapsed)
  }
  logmsg("Genetic programming evolution run FINISHED after %i evolution steps and %g seconds.",
         stepNumber, timeElapsed)
  
  structure(list(fitnessFunction = fitnessFunction,
                 stopCondition = stopCondition,
                 population = pop,
                 functionSet = functionSet,
                 constantSet = constantSet,
                 crossoverFunction = crossoverFunction,
                 mutationFunction = mutatefunc), class = "geneticProgrammingModel")
}

##' Symbolic regression via untyped genetic programming
##'
##' Perform symbolic regression via untyped genetic programming. The regression
##' task is specified as a \code{\link{formula}}. Only the simple formulas
##' without interactions are supported at this time. The result of the symbolic
##' regression run is a symbolic regression model containing an untyped GP
##' population of model functions.
##'
##' @param formula A \code{\link{formula}} describing the regression task. Only
##'   simple formulas of the form \code{response ~ variable1 + ... + variableN}
##'   are supported at this point in time.
##' @param data A \code{\link{data.frame}} containing training data for the
##'   symbolic regression run. The variables in \code{formula} must match
##'   column names in this data frame.
##' @param stopCondition The stop condition for the evolution main loop. See
##'   \link{makeStepsStopCondition} for details.
##' @param population The GP population to start the run with. If this parameter
##'   is missing, a new GP population of size \code{populationSize} is created
##'   through random growth.
##' @param populationSize The number of individuals if a population is to be
##'   created.
##' @param individualSizeLimit Individuals with a number of tree nodes that
##'   exceeds this size limit will get a fitness of \code{Inf}.
##' @param penalizeGenotypeConstantIndividuals Individuals that do not contain
##'   any input variables will get a fitness of \code{Inf}.
##' @param functionSet The function set.
##' @param constantSet The set of constant factory functions.
##' @param selectionFunction The selection function to use. Defaults to
##'   \code{tournamentSelection}. See \link{tournamentSelection} for details.
##' @param crossoverFunction The crossover function.
##' @param mutationFunction The mutation function.
##' @param progressMonitor A function of signature
##'   \code{function(population, stepNumber, timeElapsed)} to be called
##'   with each evolution step.
##' @param verbose Whether to print progress messages.
##' @return An symbolic regression model that contains an untyped GP population.
##'
##' @seealso \code{\link{predict.symbolicRegressionModel}}
##' @seealso \code{\link{geneticProgramming}}
##' @export
symbolicRegression <- function(formula, data,
                               stopCondition = makeStepsStopCondition(1000),
                               population = NULL,
                               populationSize = 500,
                               individualSizeLimit = 64,
                               penalizeGenotypeConstantIndividuals = FALSE,
                               functionSet = mathFunctionSet,
                               constantSet = numericConstantSet,
                               selectionFunction = tournamentSelection,
                               crossoverFunction = crossover,
                               mutationFunction = NULL,
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
  fitFunc <- makeRegressionFitnessFunction(formula(mf), mf, errormeasure = rmse,
                                           penalizeGenotypeConstantIndividuals = penalizeGenotypeConstantIndividuals,
                                           indsizelimit = individualSizeLimit)
  gpModel <- geneticProgramming(fitFunc, stopCondition, population, populationSize,
                                functionSet, inVarSet, constantSet, selectionFunction,
                                crossoverFunction, mutationFunction,
                                progressMonitor, verbose)
  
  structure(append(gpModel, list(formula = formula(mf))),
                   class = c("symbolicRegressionModel", "geneticProgrammingModel"))
}

##' Predict method for symbolic regression models
##'
##' Predict values via a model function from a population of model functions
##' generated by symbolic regression.
##'
##' @param object A model created by \code{\link{symbolicRegression}}.
##' @param newdata A \code{\link{data.frame}} containing input data for the
##'   symbolic regression model. The variables in \code{object$formula} must match
##'   column names in this data frame.
##' @param model The numeric index of the model function in \code{object$population}
##'   to use for prediction or \code{"BEST"} to use the model function with the best
##'   training fitness.
##' @param detailed Whether to add metadata to the prediction object returned.
##' @param ... Ignored in this \code{predict} method.
##' @return A vector of predicted values or, if \code{detailed} is \code{TRUE}, a
##'   list of the following elements:
##'   \code{model} the model used in this prediction
##'   \code{response} a matrix of predicted versus respone values
##'   \code{RMSE} the RMSE between the real and predicted response
##'
##' @export
predict.symbolicRegressionModel <- function(object, newdata, model = "BEST", detailed = FALSE, ...) {
  ind <- if (model == "BEST") {
    trainingFitnessSortedPopulation <- sortBy(object$population, object$fitnessFunction)
    trainingFitnessSortedPopulation[[1]]
  } else object$population[[model]]
  data <- if (any(is.na(newdata))) {
    dataWithoutNAs <- na.omit(newdata)
    warning(sprintf("removed %i data rows containing NA values", length(attr(dataWithoutNAs, "na.action"))))
    dataWithoutNAs
  } else newdata
  
  formulaVars <- as.list(attr(terms(object$formula), "variables")[-1])
  responseVariable <- formulaVars[[1]]
  explanatoryVariables <- formulaVars[-1]
  attach(data)
  trueResponse <- eval(responseVariable)
  explanatories <- lapply(explanatoryVariables, eval)
  detach(data)
  ysind <- do.call(ind, explanatories) # vectorized evaluation
  errorind <- rmse(trueResponse, ysind)
  
  if (detailed) {
    predictedVersusReal <- cbind(ysind, trueResponse)
    colnames(predictedVersusReal) <- c("predicted", "real")
    list(model = ind, response = predictedVersusReal, RMSE = errorind)
  } else ysind
}

##' Evolution stop conditions
##'
##' Evolution stop conditions are predicates (functions that return a single logical value)
##' of the signature \code{function(population, stepNumber, timeElapsed)}. They are used
##' to decide when to finish a GP evolution run. Stop conditions must be members of the
##' S3 class \code{c("stopCondition", "function")}. They can be combined using the generic
##' \emph{and} (\code{|}), \emph{or} (\code{|}) and \emph{not} (\code{!}) functions.
##'
##' \code{makeStepsStopCondition} creates a stop condition that is fulfilled if the number
##' of evolution steps exceeds a given limit.
##' \code{makeTimeStopCondition} creates a stop condition that is fulfilled if the run time
##' (in seconds) of an evolution run exceeds a given limit.
##'
##' @param stepLimit The maximum number of evolution steps for \code{makeStepsStopCondition}.
##' @param timeLimit The maximum runtime in seconds for \code{makeTimeStopCondition}.
##' @param e1 A stop condition.
##' @param e2 A stop condition.
##'
##' @rdname evolutionStopConditions
##' @export
makeStepsStopCondition <- function(stepLimit) {
  stopCondition <- function(pop, stepNumber, timeElapsed) stepNumber >= stepLimit
  class(stopCondition) <- c("stopCondition", "function")
  stopCondition
}

##' @rdname evolutionStopConditions
##' @export
makeTimeStopCondition <- function(timeLimit) {
  stopCondition <- function(pop, stepNumber, timeElapsed) timeElapsed >= timeLimit
  class(stopCondition) <- c("stopCondition", "function")
  stopCondition
}

##' @rdname evolutionStopConditions
##' @export `&.stopCondition`
`&.stopCondition` <- function(e1, e2) {
  stopCondition <- function(pop, stepNumber, timeElapsed)
    e1(pop, stepNumber, timeElapsed) && e2(pop, stepNumber, timeElapsed)
  class(stopCondition) <- c("stopCondition", "function")
  stopCondition
}

##' @rdname evolutionStopConditions
##' @export `|.stopCondition`
`|.stopCondition` <- function(e1, e2) {
  stopCondition <- function(pop, stepNumber, timeElapsed)
    e1(pop, stepNumber, timeElapsed) || e2(pop, stepNumber, timeElapsed)
  class(stopCondition) <- c("stopCondition", "function")
  stopCondition
}

##' @rdname evolutionStopConditions
##' @export `!.stopCondition`
`!.stopCondition` <- function(e1) {
  stopCondition <- function(pop, stepNumber, timeElapsed)
    !e1(pop, stepNumber, timeElapsed)
  class(stopCondition) <- c("stopCondition", "function")
  stopCondition
}

##' Some simple arithmetic and logic functions for use in GP expressions
##'
##' \code{safeDivide} a division operator that returns 0 if the divisor is 0.
##' \code{safeLn} a natural logarithm operator that return 0 if its argument is less
##'   then 0.
##' \code{ln} is the natural logarithm.
##' \code{positive} returns true if its argument is greater then 0.
##' \code{ifPositive} returns its second argument if its first argument is positive,
##'   otherwise its third argument.
##' \code{ifThenElse} returns its second argument if its first argument is \code{TRUE},
##'   otherwise its third argument.
##'
##' @param a A numeric value.
##' @param b A numeric value.
##' @param x A numeric value.
##' @param thenbranch The element to return when \code{x} is \code{TRUE}.
##' @param elsebranch The element to return when \code{x} is \code{FALSE}.
##'
##' @rdname safeGPfunctions
##' @export
safeDivide <- function(a, b) ifelse(b == 0, b, a / b)

##' @rdname safeGPfunctions
##' @export
safeSqroot <- function(a) sqrt(ifelse(a < 0, 0, a))

##' @rdname safeGPfunctions
##' @export
safeLn <- function(a) log(ifelse(a < 0, 0, a))

##' @rdname safeGPfunctions
##' @export
ln <- function(a) log(a)

##' @rdname safeGPfunctions
##' @export
positive <- function(x) x > 0

##' @rdname safeGPfunctions
##' @export
ifPositive <- function(x, thenbranch, elsebranch) ifelse(x > 0, thenbranch, elsebranch)

##' @rdname safeGPfunctions
##' @export
ifThenElse <- function(x, thenbranch, elsebranch) ifelse(x, thenbranch, elsebranch)

##' Default function- and constant factory sets for Genetic Programming
##'
##' \code{arithmeticFunctionSet} is an untyped function set containing the functions
##' "+", "-", "*", and "/".
##' \code{expLogFunctionSet} is an untyped function set containing the functions
##' "sqrt", "exp", and "ln".
##' \code{trigonometricFunctionSet} is an untyped function set containing the functions
##' "sin", "cos", and "tan".
##' \code{mathFunctionSet} is an untyped function set containing all the above functions.
##'
##' \code{numericConstantSet} is an untyped constant factory set containing a single
##' constant factory that creates numeric constants via calls to \code{runif(1, -1, 1)}.
##'
##' @rdname defaultGPFunctionAndConstantSets
##' @export
arithmeticFunctionSet <- functionSet("+", "-", "*", "/")

##' @rdname defaultGPFunctionAndConstantSets
##' @export
expLogFunctionSet <- functionSet("sqrt", "exp", "ln")

##' @rdname defaultGPFunctionAndConstantSets
##' @export
trigonometricFunctionSet <- functionSet("sin", "cos", "tan")

##' @rdname defaultGPFunctionAndConstantSets
##' @export
mathFunctionSet <- c(arithmeticFunctionSet, expLogFunctionSet, trigonometricFunctionSet)

##' @rdname defaultGPFunctionAndConstantSets
##' @export
numericConstantSet <- constantFactorySet(function() runif(1, -1, 1))
