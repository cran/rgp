\name{selectionFunctions}
\alias{tournamentSelection}
\title{GP selection functions...}
\usage{tournamentSelection(population, fitnessFunction, tournamentSize=2, tournamentDeterminism=1)
}
\description{GP selection functions}
\details{A GP selection function determines which individuals in a population should
survive, i.e. are selected for variation or cloning. Single-objective selection
functions only base their selection decision on a single fitness function,
whereas multi-objective selection functions support multiple fitness functions.
Every selection function at least takes a population and a (list of) fitness
functions as required arguments and returns a list containing the index of
the selected individual and a table with columns \code{index} and
\emph{fitness}, where the last column contains the value of the first fitness
function in the argument \code{fitnessFunction}, of all individuals whose
fitness where evaluated during selection.

\code{tournamentSelection} implements classic single-objective tournament selection.}
\value{A list with the following two components:
\code{selectedIndex} the index of the selected individual in \code{population}
\code{fitnessValues} a table of index and fitness of each individual that was
evaluated in the course of the selection procedure}
\arguments{\item{population}{The population to select from. All indices returned refer to
this population.}
\item{fitnessFunction}{A single fitness function for single-objective selection
functions.}
\item{tournamentSize}{The number of individuals to randomly select to form a
tournament.}
\item{tournamentDeterminism}{The propability \emph{p} for selecting the best individual
in a tournament, must be in the interval (0.0, 1.0]. The best individual is selected
with propability \emph{p}, the second best individual is selected with propability
\emph{p * (1 - p)}, the third best individual ist selected with propability
\emph{p * (1 - p)^2}, and so on. Note that setting \code{tournamentDeterminism}
to \code{1.0} (the default) yields determistic behavior.}
}

