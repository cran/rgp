\name{randfuncTyped}
\alias{randfuncTyped}
\title{Creates a well-typed R function with a random expression as its body...}
\usage{randfuncTyped(type, funcset, inset, conset, maxdepth=16,
    exprfactory=randexprTypedGrow)
}
\description{Creates a well-typed R function with a random expression as its body}
\value{A randomly generated well-typed R function.}
\arguments{\item{type}{The range type of the random function to create.}
\item{funcset}{The function set.}
\item{inset}{The set of input variables.}
\item{conset}{The set of constant factories.}
\item{maxdepth}{The maximum expression tree depth.}
\item{exprfactory}{The function to use for randomly creating the function's body.}
}

