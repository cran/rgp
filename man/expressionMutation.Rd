\name{expressionMutation}
\alias{mutateFunc}
\title{Random mutation of functions and expressions...}
\usage{mutateFunc(func, funcset, mutatefuncprob=0.01)
mutateSubtree(func, funcset, inset, conset, mutatesubtreeprob=0.1, maxsubtreedepth=5)
mutateNumericConst(func, mutateconstprob=0.1)
mutateFuncTyped(func, funcset, mutatefuncprob=0.01)
mutateSubtreeTyped(func, funcset, inset, conset, mutatesubtreeprob=0.1, maxsubtreedepth=5)
mutateNumericConstTyped(func, mutateconstprob=0.1)
}
\description{Random mutation of functions and expressions}
\details{\code{mutateFunc}: \code{mutateFunc} mutates a function \eqn{f} by recursively replacing inner functions in
\eqn{f} with probability \code{mutatefuncprob}.
\code{mutateSubtree} mutates a function by recursively replacing inner nodes with
newly grown subtrees of maximum depth \code{maxsubtreedepth}.
\code{mutateNumericConst} mutates a function by perturbing each numeric constant \eqn{c}
with probability \code{mutateconstprob} by setting \eqn{c := c + rnorm(1)}.
\code{mutateFuncTyped}, \code{mutateSubtreeTyped}, and \code{mutateNumericConstTyped} are
variants of the above functions that respect sType tags and only create well-typed results.

}
\value{\code{mutateFunc}: The randomly mutated function.

}
\arguments{\item{func}{The function to mutate randomly.}
\item{funcset}{The function set.}
\item{inset}{The set of input variables.}
\item{conset}{The set of constant factories.}
\item{mutatefuncprob}{The probability of trying to replace an inner function at each node.}
\item{mutatesubtreeprob}{The probability of replacing a subtree with a newly grown subtree
at each node.}
\item{maxsubtreedepth}{The maximum depth of newly grown subtrees.}
\item{mutateconstprob}{The probability of mutating a constant by adding \code{rnorm(1)} to it.}
}
\alias{mutateSubtree}
\alias{mutateNumericConst}
\alias{mutateFuncTyped}
\alias{mutateSubtreeTyped}
\alias{mutateNumericConstTyped}

