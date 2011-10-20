\name{randomFunctionCreation}
\alias{randfunc}
\title{Creates an R function with a random expression as its body...}
\usage{randfunc(funcset, inset, conset, maxdepth=8, constprob=0.2,
    exprfactory=randexprGrow, breedingFitness=function(individual)
    TRUE, breedingTries=50)
randfuncRampedHalfAndHalf(funcset, inset, conset, maxdepth=8, constprob=0.2,
    breedingFitness=function(individual) TRUE, breedingTries=50)
}
\description{Creates an R function with a random expression as its body}
\value{\code{randfunc}: A randomly generated R function.

}
\arguments{\item{funcset}{The function set.}
\item{inset}{The set of input variables.}
\item{conset}{The set of constant factories.}
\item{maxdepth}{The maximum expression tree depth.}
\item{exprfactory}{The function to use for randomly creating the function's body.}
\item{constprob}{The probability of generating a constant in a step of growth, if no subtree
is generated. If neither a subtree nor a constant is generated, a randomly chosen input variable
will be generated. Defaults to \code{0.2}.}
\item{breedingFitness}{A breeding function. See the documentation for
\code{\link{geneticProgramming}} for details.}
\item{breedingTries}{The number of breeding steps.}
}
\alias{randfuncRampedHalfAndHalf}

