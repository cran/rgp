\name{randomFunctionCreationTyped}
\alias{randfuncTyped}
\title{Creates a well-typed R function with a random expression as its body...}
\usage{randfuncTyped(type, funcset, inset, conset, maxdepth=8, constprob=0.2,
    exprfactory=randexprTypedGrow)
randfuncTypedRampedHalfAndHalf(type, funcset, inset, conset, maxdepth=8, constprob=0.2)
}
\description{Creates a well-typed R function with a random expression as its body}
\value{\code{randfuncTyped}: A randomly generated well-typed R function.

}
\arguments{\item{type}{The range type of the random function to create.}
\item{funcset}{The function set.}
\item{inset}{The set of input variables.}
\item{conset}{The set of constant factories.}
\item{maxdepth}{The maximum expression tree depth.}
\item{constprob}{The probability of generating a constant in a step of growth, if no subtree
is generated. If neither a subtree nor a constant is generated, a randomly chosen input variable
will be generated. Defaults to \code{0.2}.}
\item{exprfactory}{The function to use for randomly creating the function's body.}
}
\alias{randfuncTypedRampedHalfAndHalf}

