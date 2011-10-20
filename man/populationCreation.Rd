\name{populationCreation}
\alias{makePopulation}
\title{Classes for populations of individuals represented as functions...}
\usage{makePopulation(size, funcset, inset, conset, maxfuncdepth=8, constprob=0.2,
    breedingFitness=function(individual) TRUE, breedingTries=50,
    extinctionPrevention=FALSE, funcfactory=function()
    randfuncRampedHalfAndHalf(funcset, inset, conset, maxfuncdepth,
    constprob = constprob, breedingFitness = breedingFitness,
    breedingTries = breedingTries))
makeTypedPopulation(size, type, funcset, inset, conset, maxfuncdepth=8, constprob=0.2,
    breedingFitness=function(individual) TRUE, breedingTries=50,
    extinctionPrevention=FALSE, funcfactory=function()
    randfuncTypedRampedHalfAndHalf(type, funcset, inset, conset,
    maxfuncdepth, constprob = constprob, breedingFitness =
    breedingFitness, breedingTries = breedingTries))
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
\item{constprob}{The probability of generating a constant in a step of growth, if no subtree
is generated. If neither a subtree nor a constant is generated, a randomly chosen input variable
will be generated. Defaults to \code{0.2}.}
\item{breedingFitness}{A breeding function. See the documentation for
\code{\link{geneticProgramming}} for details.}
\item{breedingTries}{The number of breeding steps.}
\item{extinctionPrevention}{When set to \code{TRUE}, initialization will try 
to prevent duplicate individuals from occurring in the population. Defaults to \code{FALSE}, as
this operation might be expensive with larger population sizes.}
\item{funcfactory}{A factory for creating the functions of the new population.
Defaults to Koza's "ramped half-and-half" initialization strategy.}
\item{x}{The population to print.}
\item{object}{The population to summarize.}
\item{...}{Additional parameters to the \code{\link{print}} or \code{\link{summary}}
(passed on to their default implementation).}
}
\alias{makeTypedPopulation}
\alias{print.population}
\alias{summary.population}

