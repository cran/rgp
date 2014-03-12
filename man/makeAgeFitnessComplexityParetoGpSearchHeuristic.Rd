\name{makeAgeFitnessComplexityParetoGpSearchHeuristic}
\alias{makeAgeFitnessComplexityParetoGpSearchHeuristic}
\title{Age Fitness Complexity Pareto GP Search Heuristic for RGP}
\usage{
makeAgeFitnessComplexityParetoGpSearchHeuristic(lambda = 50,
  crossoverProbability = 0.5, enableComplexityCriterion = TRUE,
  enableAgeCriterion = FALSE, ndsParentSelectionProbability = 0,
  ndsSelectionFunction = nds_cd_selection, complexityMeasure = function(ind,
  fitness) fastFuncVisitationLength(ind), ageMergeFunction = max,
  newIndividualsPerGeneration = if (enableAgeCriterion) 50 else 0,
  newIndividualsMaxDepth = 8, newIndividualFactory = makePopulation)
}
\arguments{
  \item{lambda}{The number of children to create in each
  generation (\code{50} by default).}

  \item{crossoverProbability}{The crossover probability for
  search-heuristics that support this setting (i.e.
  TinyGP). Defaults to \code{0.5}.}

  \item{enableComplexityCriterion}{Whether to enable the
  complexity criterion in multi-criterial search
  heuristics.}

  \item{enableAgeCriterion}{Whether to enable the age
  criterion in multi-criterial search heuristics.}

  \item{ndsParentSelectionProbability}{The probability to
  use non-dominated sorting to select parents for each
  generation. When set to \code{0.0}, parents are selected
  by uniform random sampling without replacement every
  time. Defaults to \code{1.0}.}

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
  Defaults to \code{50} if \code{enableAgeCriterion ==
  TRUE} else to \code{0}.}

  \item{newIndividualsMaxDepth}{The maximum depth of new
  individuals inserted into the population.}

  \item{newIndividualFactory}{The factory function for
  creating new individuals. Defaults to
  \code{makePopulation}.}
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
algorithm (EMOA) that selects on three criteria: Individual
age, individual fitness, and individual complexity.
}

