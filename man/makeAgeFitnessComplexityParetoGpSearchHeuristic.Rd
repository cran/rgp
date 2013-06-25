\name{makeAgeFitnessComplexityParetoGpSearchHeuristic}
\alias{makeAgeFitnessComplexityParetoGpSearchHeuristic}
\title{Age Fitness Complexity Pareto GP Search Heuristic for RGP}
\usage{
  makeAgeFitnessComplexityParetoGpSearchHeuristic(lambda = 20,
    crossoverProbability = 0.9,
    enableComplexityCriterion = FALSE,
    enableAgeCriterion = FALSE, ndsParentSelection = FALSE,
    ndsSelectionFunction = nds_cd_selection,
    complexityMeasure = function(ind, fitness) funcVisitationLength(ind),
    ageMergeFunction = max,
    newIndividualsPerGeneration = if (enableAgeCriterion) 2 else 0,
    newIndividualsMaxDepth = 8,
    newIndividualFactory = makePopulation,
    plotFront = FALSE)
}
\arguments{
  \item{lambda}{The number of children to create in each
  generation.}

  \item{crossoverProbability}{The crossover probability for
  search-heuristics that support this setting (i.e.
  TinyGP). Defaults to \code{0.9}.}

  \item{enableComplexityCriterion}{Whether to enable the
  complexity criterion in multi-criterial search
  heuristics.}

  \item{enableAgeCriterion}{Whether to enable the age
  criterion in multi-criterial search heuristics.}

  \item{ndsParentSelection}{Whether to use non-dominated
  sorting to select parents. When set to \code{FALSE},
  parents are selected by uniform random sampling without
  replacement.}

  \item{ndsSelectionFunction}{The function to use for
  non-dominated sorting in Pareto GP selection.  Defaults
  to \code{nds_cd_selection}.}

  \item{complexityMeasure}{The complexity measure, a
  function of signature \code{function(ind, fitness)}
  returning a single numeric value.}

  \item{ageMergeFunction}{The function used for merging
  ages of crossover children, defaults to \code{max}.}

  \item{newIndividualsPerGeneration}{The number of new
  individuals per generation to insert into the population.
  Defaults to \code{2} if \code{enableAgeCriterion == TRUE}
  else to \code{0}.}

  \item{newIndividualsMaxDepth}{The maximum depth of new
  individuals inserted into the population.}

  \item{newIndividualFactory}{The factory function for
  creating new individuals. Defaults to
  \code{makePopulation}.}

  \item{plotFront}{Whether to plot the pareto front during
  GP runs (for monitoring and debugging).}
}
\value{
  An RGP search heuristic.
}
\description{
  The search-heuristic, i.e. the concrete GP search
  algorithm, is a modular component of RGP.
  \code{makeAgeFitnessComplexityParetoGpSearchHeuristic}
  creates a RGP search-heuristic that implements a
  generational evolutionary multi objective optimization
  algorithm (EMOA) that selects on three criteria:
  Individual age, individual fitness, and individual
  complexity.
}

