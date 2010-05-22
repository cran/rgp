\name{geneticProgramming}
\alias{geneticProgramming}
\title{Genetic programming run...}
\usage{geneticProgramming(fitnessFunction, stopCondition=makeTimeStopCondition(5), population,
    populationSize=500, functionSet=mathFunctionSet,
    inputVariables=inputVariableSet("x"),
    constantSet=numericConstantSet,
    selectionFunction=tournamentSelection, crossoverFunction=crossover,
    mutationFunction, progressMonitor, verbose=TRUE)
}
\description{Genetic programming run}
\details{Perform a genetic programming run. The required argument \code{fitnessFunction}
must be supplied with an objective function that assigns a numerical fitness
value to an R function. Fitness values are minimized, i.e. smaller values mean
higher/better fitness. If a multi-objective \code{selectionFunction} is
used, \code{fitnessFunction} must be a list of objective functions.
The result of the genetic programming run is a genetic programming model
containing a GP population of R functions.}
\value{A genetic programming model that contains a GP population in the
field \code{population}, as well as metadata describing the run parameters.}
\arguments{\item{fitnessFunction}{In case of a single-objective selection function,
\code{fitnessFunction} must be a single function that assigns a
numerical fitness value to a GP individual represented as a R function.
Smaller fitness values mean higher/better fitness. If a multi-objective
selection function is used, \code{fitnessFunction} must be a list of
of objective functions.}
\item{stopCondition}{The stop condition for the evolution main loop. See
\link{makeStepsStopCondition} for details.}
\item{population}{The GP population to start the run with. If this parameter
is missing, a new GP population of size \code{populationSize} is created
through random growth.}
\item{populationSize}{The number of individuals if a population is to be
created.}
\item{functionSet}{The function set.}
\item{inputVariables}{The input variable set.}
\item{constantSet}{The set of constant factory functions.}
\item{selectionFunction}{The selection function to use. Defaults to
\code{tournamentSelection}. See \link{tournamentSelection} for details.}
\item{crossoverFunction}{The crossover function.}
\item{mutationFunction}{The mutation function.}
\item{progressMonitor}{A function of signature
\code{function(population, stepNumber, timeElapsed)} to be called
with each evolution step.}
\item{verbose}{Whether to print progress messages.}
}

