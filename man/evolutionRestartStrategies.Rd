\name{evolutionRestartStrategies}
\alias{makeLocalRestartStrategy}
\title{Evolution restart strategies...}
\usage{makeLocalRestartStrategy(populationType)
}
\description{Evolution restart strategies}
\details{Evolution restart strategies are functions of the signature \code{function(fitnessFunction,
population, populationSize, functionSet, inputVariables, constantSet)} that return a list of
two obtjects: First, a population that replace the run's current population. Second, a list
of elite individuals to keep.

\code{makeLocalRestartStrategy} creates a restart strategy that replaces all individuals with
new individuals. The single best individual is returned as the elite. When using a
multi-criterial fitness function, only the first component counts in the fitness sorting.}
\arguments{\item{populationType}{The sType of the replacement individuals, defaults to \code{NULL} for
creating untyped populations.}
}

