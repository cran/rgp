## fitness.r
##   - Standard GP fitness functions and tools for creating GP fitness functions
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Mean squared error (MSE)
##'
##' @param x A numeric vector or list.
##' @param y A numeric vector or list.
##' @return The MSE between \code{x} and \code{y}.
##' @export
mse <- function(x, y) mean((x - y)^2)

##' Root mean squared error (RMSE)
##'
##' @param x A numeric vector or list.
##' @param y A numeric vector or list.
##' @return The RMSE between \code{x} and \code{y}.
##' @export
rmse <- function(x, y) sqrt(mse(x, y))

##' Normalize a vector into the interval [0, 1]
##'
##' @param x The vector to normalize, so that each element lies in the
##'   interval [0, 1].
##' @return The normalized vector.
##' @export
normalize <- function(x) (x - min(x)) / (max(x) - min(x))

##' Scaled mean squared error (SMSE)
##'
##' Calculates the MSE between vectors after normalizing them into the
##' interval [0, 1].
##'
##' @param x A numeric vector or list.
##' @param y A numeric vector or list.
##' @return The SMSE between \code{x} and \code{y}.
smse <- function(x, y) mse(normalize(x), normalize(y))

##' Create a fitness function from a function of one variable
##'
##' Creates a fitness function that calculates an error measure with
##' respect to an arbitrary reference function of one variable on the
##' sequence of fitness cases \code{seq(from, to, length = steps)}.
##' When an \code{indsizelimit} is given, individuals exceeding this
##' limit will receive a fitness of \code{Inf}.
##'
##' @param func The reference function.
##' @param from The start of the sequence of fitness cases.
##' @param to The end of the sequence of fitness cases.
##' @param steps The number of steps in the sequence of fitness cases.
##' @param errormeasure A function to use as an error measure.
##' @param indsizelimit Individuals exceeding this size limit will get
##'   a fitness of \code{Inf}.
##' @return A fitness function based on the reference function \code{func}.
##' @export
makeFunctionFitnessFunction <- function(func, from = -1, to = 1, steps = 128,
                                        errormeasure = rmse, indsizelimit = NA) {
  xs <- seq(from, to, length = steps)
  ystarget <- func(xs)
  function(ind) {
    ysind <- ind(xs) # vectorized fitness-case evaluation
  	errorind <- errormeasure(ystarget, ysind)
  	if (!is.na(indsizelimit) && funcSize(ind) > indsizelimit)
  	  Inf # ind size limit exceeded
  	else if (is.na(errorind) || is.nan(errorind))
  	  Inf # error value is NA or NaN
  	else errorind
  }
}

##' Create a fitness function for symbolic regression
##'
##' Creates a fitness function that calculates an error measure with
##' respect to a given set of data variables. A simplified version of
##' the formula syntax is used to describe the regression task. When
##' an \code{indsizelimit} is given, individuals exceeding this limit
##' will receive a fitness of \code{Inf}.
##'
##' @param formula A formula object describing the regression task.
##' @param data An optional data frame containing the variables in the
##'   model.
##' @param errormeasure A function to use as an error measure.
##' @param indsizelimit Individuals exceeding this size limit will get
##'   a fitness of \code{Inf}.
##' @param penalizeGenotypeConstantIndividuals Individuals that do not
##'   contain any input variables will get a fitness of \code{Inf}.
##' @return A fitness function to be used in symbolic regression.
##' @export
makeRegressionFitnessFunction <- function(formula, data, errormeasure = rmse,
                                          indsizelimit = NA,
                                          penalizeGenotypeConstantIndividuals = FALSE) {
  data <- if (any(is.na(data))) {
    dataWithoutNAs <- na.omit(data)
    warning(sprintf("removed %i data rows containing NA values",
                    length(attr(dataWithoutNAs, "na.action"))))
    dataWithoutNAs
  } else data
  formulaVars <- as.list(attr(terms(formula), "variables")[-1])
  responseVariable <- formulaVars[[1]]
  explanatoryVariables <- formulaVars[-1]
  trueResponse <- eval(responseVariable, envir=data)
  explanatories <- lapply(explanatoryVariables, eval, envir=data)
  function(ind) {
    ysind <- do.call(ind, explanatories) # vectorized fitness-case evaluation
    errorind <- errormeasure(trueResponse, ysind)    
    if (!is.na(indsizelimit) && funcSize(ind) > indsizelimit)
      Inf # individual size limit exceeded
    else if (is.na(errorind) || is.nan(errorind))
      Inf # error value is NA or NaN
    else if (penalizeGenotypeConstantIndividuals
             && is.empty(inputVariablesOfIndividual(ind, explanatoryVariables)))
      Inf # individual does not contain any input variables
    else errorind
  }
}
