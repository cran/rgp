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
##' @param errorMeasure A function to use as an error measure, defaults to RMSE.
##' @param indsizelimit Individuals exceeding this size limit will get
##'   a fitness of \code{Inf}.
##' @return A fitness function based on the reference function \code{func}.
##' @export
makeFunctionFitnessFunction <- function(func, from = -1, to = 1, steps = 128,
                                        errorMeasure = rmse, indsizelimit = NA) {
  xs <- seq(from, to, length = steps)
  ystarget <- func(xs)
  function(ind) {
    ysind <- ind(xs) # vectorized fitness-case evaluation
  	errorind <- errorMeasure(ystarget, ysind)
  	if (!is.na(indsizelimit) && funcSize(ind) > indsizelimit)
  	  Inf # ind size limit exceeded
  	else if (is.na(errorind) || is.nan(errorind))
  	  Inf # error value is NA or NaN
  	else errorind
  }
}
