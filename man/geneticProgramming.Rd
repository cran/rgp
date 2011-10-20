\name{geneticProgramming}
\alias{geneticProgramming}
\title{Standard typed and untyped genetic programming...}
\usage{geneticProgramming(fitnessFunction, stopCondition=makeTimeStopCondition(5), population,
    populationSize=100, eliteSize=ceiling(0.1 * populationSize),
    elite=list(), functionSet=mathFunctionSet,
    inputVariables=inputVariableSet("x"),
    constantSet=numericConstantSet,
    selectionFunction=makeTournamentSelection(),
    crossoverFunction=crossover, mutationFunction,
    restartCondition=makeEmptyRestartCondition(),
    restartStrategy=makeLocalRestartStrategy(),
    breedingFitness=function(individual) TRUE, breedingTries=50,
    extinctionPrevention=FALSE, archive=FALSE, genealogy=FALSE,
    progressMonitor, verbose=TRUE)
typedGeneticProgramming(fitnessFunction, type, stopCondition=makeTimeStopCondition(5),
    population, populationSize=100, eliteSize=ceiling(0.1 *
    populationSize), elite=list(), functionSet, inputVariables,
    constantSet, selectionFunction=makeTournamentSelection(),
    crossoverFunction=crossoverTyped, mutationFunction,
    restartCondition=makeEmptyRestartCondition(),
    restartStrategy=makeLocalRestartStrategy(populationType = type),
    breedingFitness=function(individual) TRUE, breedingTries=50,
    extinctionPrevention=FALSE, archive=FALSE, genealogy=FALSE,
    progressMonitor, verbose=TRUE)
}
\description{Standard typed and untyped genetic programming}
\details{\code{geneticProgramming}: Perform a standard genetic programming (GP) run. Use \code{geneticProgramming} for
untyped genetic programming or \code{typedGeneticProgramming} for typed genetic
programming runs. The required argument \code{fitnessFunction} must be supplied with
an objective function that assigns a numerical fitness value to an R function. Fitness
values are minimized, i.e. smaller values denote higher/better fitness. If a
multi-objective \code{selectionFunction} is used, \code{fitnessFunction} return a
numerical vector of fitness values. The result of the GP run is a GP result object
containing a GP population of R functions. \code{summary.geneticProgrammingResult} can
be used to create summary views of a GP result object. During the run, restarts are
triggered by the \code{restartCondition}. When a restart is triggered, the restartStrategy
is executed, which returns a new population to replace the current one as well as a list of
elite individuals. These are added to the runs elite list, where fitter individuals replace
individuals with lesser fittness. The runs elite list is always sorted by fitness in
ascending order. Only the first component of a multi-criterial fitness counts in this
sorting. After a GP run, the population is inserted into the elite list. The elite list
is returned as part of the GP result object.

}
\value{\code{geneticProgramming}: A genetic programming result object that contains a GP population in the
field \code{population}, as well as metadata describing the run parameters.

}
\seealso{\code{\link{summary.geneticProgrammingResult}}, \code{\link{symbolicRegression}}}
\arguments{\item{fitnessFunction}{In case of a single-objective selection function,
\code{fitnessFunction} must be a single function that assigns a
numerical fitness value to a GP individual represented as a R function.
Smaller fitness values mean higher/better fitness. If a multi-objective
selection function is used, \code{fitnessFunction} must return a numerical
vector of fitness values.}
\item{type}{The range type of the individual functions. This parameter
only applies to \code{typedGeneticProgramming}.}
\item{stopCondition}{The stop condition for the evolution main loop. See
\link{makeStepsStopCondition} For details.}
\item{population}{The GP population to start the run with. If this parameter
is missing, a new GP population of size \code{populationSize} is created
through random growth.}
\item{populationSize}{The number of individuals if a population is to be
created.}
\item{eliteSize}{The number of elite individuals to keep. Defaults to
\code{ceiling(0.1 * populationSize)}.}
\item{elite}{The elite list, must be alist of individuals sorted in ascending
order by their first fitness component.}
\item{functionSet}{The function set.}
\item{inputVariables}{The input variable set.}
\item{constantSet}{The set of constant factory functions.}
\item{selectionFunction}{The selection function to use. Defaults to
tournament selection. See \link{makeTournamentSelection} for details.}
\item{crossoverFunction}{The crossover function.}
\item{mutationFunction}{The mutation function.}
\item{restartCondition}{The restart condition for the evolution main loop. See
\link{makeEmptyRestartCondition} for details.}
\item{restartStrategy}{The strategy for doing restarts. See
\link{makeLocalRestartStrategy} for details.}
\item{breedingFitness}{A "breeding" function. This function is applied after
every stochastic operation \emph{Op} that creates or modifies an individal
(typically, \emph{Op} is a initialization, mutation, or crossover operation). If
the breeding function returns \code{TRUE} on the given individual, \emph{Op} is
considered a success. If the breeding function returns \code{FALSE}, \emph{Op}
is retried a maximum of \code{breedingTries} times. If this maximum number of
retries is exceeded, the result of the last try is considered as the result of
\emph{Op}. In the case the breeding function returns a numeric value, the breeding
is repeated \code{breedingTries} times and the individual with the lowest breeding
fitness is considered the result of \emph{Op}.}
\item{breedingTries}{In case of a boolean \code{breedingFitness} function, the
maximum number of retries. In case of a numerical \code{breedingFitness} function,
the number of breeding steps. Also see the documentation for the \code{breedingFitness}
parameter. Defaults to \code{50}.}
\item{extinctionPrevention}{When set to \code{TRUE}, the initialization and
selection steps will try to prevent duplicate individuals
from occurring in the population. Defaults to \code{FALSE}, as this
operation might be expensive with larger population sizes.}
\item{archive}{If set to \code{TRUE}, all GP individuals evaluated are stored in an
archive list \code{archiveList} that is returned as part of the result of this function.}
\item{genealogy}{If set to \code{TRUE}, the parent(s) of each indiviudal is stored in
an archive \code{genealogyList} as part of the result of this function, enabling
the reconstruction of the complete genealogy of the result population. This
parameter implies \code{archive = TRUE}.}
\item{progressMonitor}{A function of signature
\code{function(population, fitnessfunction, stepNumber, evaluationNumber,
bestFitness, timeElapsed)} to be called with each evolution step.}
\item{verbose}{Whether to print progress messages.}
}
\alias{typedGeneticProgramming}

