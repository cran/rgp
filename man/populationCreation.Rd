\name{populationCreation}
\alias{makePopulation}
\title{Classes for populations of individuals represented as functions...}
\usage{makePopulation(size, funcset, inset, conset, maxfuncdepth=8, funcfactory=function()
    randfunc(funcset, inset, conset, maxfuncdepth))
makeTypedPopulation(size, type, funcset, inset, conset, maxfuncdepth=8,
    funcfactory=function() randfuncTyped(type, funcset, inset, conset,
    maxfuncdepth))
print.population(x, ...)
summary.population(object, ...)
}
\description{Classes for populations of individuals represented as functions}
\details{\code{makePopulation}: \code{makePopulation} creates a population of untyped individuals, whereas
\code{makeTypedPopulation} creates a population of typed individuals.
\code{print.population} prints the population.
\code{summary.population} returns a summary view of a population.

}
\value{\code{makePopulation}: A new population of functions.

}
\arguments{\item{size}{The population size in number of individuals.}
\item{type}{The (range) type of the individual functions to create.}
\item{funcset}{The function set.}
\item{inset}{The set of input variables.}
\item{conset}{The set of constant factories.}
\item{maxfuncdepth}{The maximum depth of the functions of the new population.}
\item{funcfactory}{A factory for creating the functions of the new population.}
\item{x}{The population to print.}
\item{object}{The population to summarize.}
\item{...}{Additional parameters to the \code{\link{print}} or \code{\link{summary}}
(passed on to their default implementation).}
}
\alias{makeTypedPopulation}
\alias{print.population}
\alias{summary.population}

