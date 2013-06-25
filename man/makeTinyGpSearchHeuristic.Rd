\name{makeTinyGpSearchHeuristic}
\alias{makeTinyGpSearchHeuristic}
\title{Tiny GP Search Heuristic for RGP}
\usage{
  makeTinyGpSearchHeuristic(crossoverProbability = 0.9,
    tournamentSize = 2)
}
\arguments{
  \item{crossoverProbability}{The crossover probability for
  search-heuristics that support this setting (i.e.
  TinyGP). Defaults to \code{0.9}.}

  \item{tournamentSize}{The size of TinyGP's selection
  tournaments.}
}
\value{
  An RGP search heuristic.
}
\description{
  The search-heuristic, i.e. the concrete GP search
  algorithm, is a modular component of RGP.
  \code{makeTinyGpSearchHeuristic} creates an RGP
  search-heuristic that mimics the search heuristic
  implemented in Riccardo Poli's TinyGP system.
}

