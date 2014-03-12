\name{makeTournamentSelection}
\alias{makeComplexityTournamentSelection}
\alias{makeMultiObjectiveTournamentSelection}
\alias{makeTournamentSelection}
\title{GP selection functions}
\usage{
makeTournamentSelection(tournamentSize = 10,
  selectionSize = ceiling(tournamentSize/2), tournamentDeterminism = 1,
  vectorizedFitness = FALSE)

makeMultiObjectiveTournamentSelection(tournamentSize = 30,
  selectionSize = ceiling(tournamentSize/2), tournamentDeterminism = 1,
  vectorizedFitness = FALSE,
  rankingStrategy = orderByParetoCrowdingDistance)

makeComplexityTournamentSelection(tournamentSize = 30,
  selectionSize = ceiling(tournamentSize/2), tournamentDeterminism = 1,
  vectorizedFitness = FALSE,
  rankingStrategy = orderByParetoCrowdingDistance,
  complexityMeasure = fastFuncVisitationLength)
}
\arguments{
  \item{complexityMeasure}{The function used to measure the
  complexity of an individual.}

  \item{tournamentSize}{The number of individuals to
  randomly select to form a tournament, defaults to 10 in
  the single-objective case, 30 in the multi-objective
  case.}

  \item{selectionSize}{The number of individuals to return
  as selected.}

  \item{tournamentDeterminism}{The propability \emph{p} for
  selecting the best individual in a tournament, must be in
  the interval (0.0, 1.0]. The best individual is selected
  with propability \emph{p}, the second best individual is
  selected with propability \emph{p * (1 - p)}, the third
  best individual ist selected with propability \emph{p *
  (1 - p)^2}, and so on. Note that setting
  \code{tournamentDeterminism} to \code{1.0} (the default)
  yields determistic behavior.}

  \item{vectorizedFitness}{If \code{TRUE}, the fitness
  function is expected to take a list of individuals as
  input and return a list of (possible vector-valued)
  fitnesses as output.}

  \item{rankingStrategy}{The strategy used to rank
  individuals based on multiple objectives.  This function
  must turn a fitness vector (one point per column) into an
  ordering permutation (similar to the one returned by
  \code{order}). Defaults to
  \code{orderByParetoCrowdingDistance}.}
}
\value{
A selection function.
}
\description{
A GP selection function determines which individuals in a
population should survive, i.e. are selected for variation
or cloning, and which individuals of a population should be
replaced. Single-objective selection functions base their
selection decision on scalar fitness function, whereas
multi-objective selection functions support vector-valued
fitness functions. Every selection function takes a
population and a (possibly vector-valued) fitness function
as required arguments. It returns a list of two tables
\code{selected} and \code{discarded}, with columns
\code{index} and \code{fitness} each. The returned list
also contains a single integer
\code{numberOfFitnessEvaluations} that contains the number
of fitness evaluations used to make the selection (Note
that in the multi-objective case, evaluating all fitness
functions once counts as a single evaluation). The first
table contains the population indices of the individuals
selected as survivors, the second table contains the
population indices of the individuals that should be
discarded and replaced. This definition simplifies the
implementation of \emph{steady-state} evolutionary
strategies where most of the individuals in a population
are unchanged in each selection step. In a GP context,
steady-state strategies are often more efficient than
generational strategies.
}
\details{
\code{makeTournamentSelection} returns a classic
single-objective tournament selection function.
\code{makeMultiObjectiveTournamentSelection} returns a
multi-objective tournament selection function that selects
individuals based on multiple objectives.
\code{makeComplexityTournamentSelection} returns a
multi-objective selection function that implements the
common case of dual-objective tournament selection with
high solution quality as the first objective and low
solution complexity as the second objective.
}

