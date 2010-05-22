\name{randfunc}
\alias{randfunc}
\title{Creates an R function with a random expression as its body...}
\usage{randfunc(funcset, inset, conset, maxdepth=16, exprfactory=randexprGrow)
}
\description{Creates an R function with a random expression as its body}
\value{A randomly generated R function.}
\arguments{\item{funcset}{The function set.}
\item{inset}{The set of input variables.}
\item{conset}{The set of constant factories.}
\item{maxdepth}{The maximum expression tree depth.}
\item{exprfactory}{The function to use for randomly creating the function's body.}
}

