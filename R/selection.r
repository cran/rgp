## selection.r
##   - GP selection strategies
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' GP selection functions
##'
##' A GP selection function determines which individuals in a population should
##' survive, i.e. are selected for variation or cloning. Single-objective selection
##' functions only base their selection decision on a single fitness function,
##' whereas multi-objective selection functions support multiple fitness functions.
##' Every selection function at least takes a population and a (list of) fitness
##' functions as required arguments and returns a list containing the index of
##' the selected individual and a table with columns \code{index} and
##' \emph{fitness}, where the last column contains the value of the first fitness
##' function in the argument \code{fitnessFunction}, of all individuals whose
##' fitness where evaluated during selection.
##'
##' \code{tournamentSelection} implements classic single-objective tournament selection.
##'
##' @param population The population to select from. All indices returned refer to
##'   this population.
##' @param fitnessFunction A single fitness function for single-objective selection
##'   functions.
##' @param tournamentSize The number of individuals to randomly select to form a
##'   tournament.
##' @param tournamentDeterminism The propability \emph{p} for selecting the best individual
##'   in a tournament, must be in the interval (0.0, 1.0]. The best individual is selected
##'   with propability \emph{p}, the second best individual is selected with propability
##'   \emph{p * (1 - p)}, the third best individual ist selected with propability
##'   \emph{p * (1 - p)^2}, and so on. Note that setting \code{tournamentDeterminism}
##'   to \code{1.0} (the default) yields determistic behavior.
##' @return A list with the following two components:
##'   \code{selectedIndex} the index of the selected individual in \code{population}
##'   \code{fitnessValues} a table of index and fitness of each individual that was
##'     evaluated in the course of the selection procedure
##'
##' @rdname selectionFunctions
##' @export
tournamentSelection <- function(population, fitnessFunction,
                                tournamentSize = 2, tournamentDeterminism = 1.0) {
  poolIdxs <- sample(length(population), tournamentSize)
  poolFitness <- sapply(population[poolIdxs], fitnessFunction)
  idxFitTable <- cbind(poolIdxs, poolFitness)
  colnames(idxFitTable) <- c("index", "fitness")
  sortedIdxFitTable <- idxFitTable[order(idxFitTable[,"fitness"]),] # sort by fitness
  rnds <- runif(tournamentSize); rnds[tournamentSize] <- -Inf # at least the last index is selected
  sortedIdxFitTableIdx <- which(rnds <= tournamentDeterminism)[1]
  selectedIdx <- sortedIdxFitTable[sortedIdxFitTableIdx, 1]
  list(selectedIndex = selectedIdx, fitnessValues = sortedIdxFitTable)
}
