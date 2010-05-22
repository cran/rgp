\name{expressionCrossover}
\alias{crossover}
\title{Random crossover of functions and expressions...}
\usage{crossover(func1, func2, crossoverprob=0.1)
crossoverexpr(expr1, expr2, crossoverprob)
crossoverTyped(func1, func2, crossoverprob=0.1)
crossoverexprTyped(expr1, expr2, crossoverprob)
}
\description{Random crossover of functions and expressions}
\details{\code{crossover}: Replace a random subtree of \code{func1} (\code{expr1}) with a random subtree of
\code{func2} (\code{expr2}) and return the resulting function (expression), i.e.
the modified \code{func1} (\code{expr1}).
\code{crossoverexpr} handles crossover of expressions instead of functions.
\code{crossoverTyped} and \code{crossoverexprTyped} only exchage replace subtress
if the sTypes of their root nodes match.

}
\value{\code{crossover}: The child function (expression).

}
\arguments{\item{expr1}{The first parent R expression.}
\item{func1}{The first parent R function.}
\item{expr2}{The second parent R expression.}
\item{func2}{The second parent R function.}
\item{crossoverprob}{The probability of crossover at each node of the first parent
function (expression).}
}
\alias{crossoverexpr}
\alias{crossoverTyped}
\alias{crossoverexprTyped}

