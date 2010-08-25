\name{symbolicRegression}
\alias{symbolicRegression}
\title{Symbolic regression via untyped genetic programming...}
\usage{symbolicRegression(formula, data, stopCondition=makeStepsStopCondition(1000), population,
    populationSize=500, individualSizeLimit=64,
    penalizeGenotypeConstantIndividuals=FALSE,
    functionSet=mathFunctionSet, constantSet=numericConstantSet,
    selectionFunction=makeTournamentSelection(),
    crossoverFunction=crossover, mutationFunction, progressMonitor,
    verbose=TRUE)
}
\description{Symbolic regression via untyped genetic programming}
\details{Perform symbolic regression via untyped genetic programming. The regression
task is specified as a \code{\link{formula}}. Only the simple formulas
without interactions are supported at this time. The result of the symbolic
regression run is a symbolic regression model containing an untyped GP
population of model functions.}
\value{An symbolic regression model that contains an untyped GP population.}
\seealso{\code{\link{predict.symbolicRegressionModel}}, \code{\link{geneticProgramming}}}
\arguments{\item{formula}{A \code{\link{formula}} describing the regression task. Only
simple formulas of the form \code{response ~ variable1 + ... + variableN}
are supported at this point in time.}
\item{data}{A \code{\link{data.frame}} containing training data for the
symbolic regression run. The variables in \code{formula} must match
column names in this data frame.}
\item{stopCondition}{The stop condition for the evolution main loop. See
\link{makeStepsStopCondition} for details.}
\item{population}{The GP population to start the run with. If this parameter
is missing, a new GP population of size \code{populationSize} is created
through random growth.}
\item{populationSize}{The number of individuals if a population is to be
created.}
\item{individualSizeLimit}{Individuals with a number of tree nodes that
exceeds this size limit will get a fitness of \code{Inf}.}
\item{penalizeGenotypeConstantIndividuals}{Individuals that do not contain
any input variables will get a fitness of \code{Inf}.}
\item{functionSet}{The function set.}
\item{constantSet}{The set of constant factory functions.}
\item{selectionFunction}{The selection function to use. Defaults to
tournament selection. See \link{makeTournamentSelection} for details.}
\item{crossoverFunction}{The crossover function.}
\item{mutationFunction}{The mutation function.}
\item{progressMonitor}{A function of signature
\code{function(population, stepNumber, timeElapsed)} to be called
with each evolution step.}
\item{verbose}{Whether to print progress messages.}
}

