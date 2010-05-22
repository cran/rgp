\name{expressionComplexityMeasures}
\alias{exprDepth}
\title{Complexity measures for R functions and expressions...}
\usage{exprDepth(expr)
funcDepth(func)
exprSize(expr)
funcSize(func)
exprVisitationLength(expr)
exprVisitationLengthRecursive(expr)
funcVisitationLength(func)
}
\description{Complexity measures for R functions and expressions}
\details{\code{exprDepth}: \code{exprDepth} returns the depth of the tree representation ("exression tree") of an R expression.
\code{funcDepth} returns the tree depth of the body expression of an R function.
\code{exprSize} returns the number of nodes in the tree of an R expression.
\code{funcSize} returns the number of nodes in the body expression tree of an R function.
\code{exprVisitationLength} returns the visitation length of the tree of an R expression.
The visitation length is the total number of nodes in all possible subtrees of a tree.
\code{funcVisitationLength} returns the visitation length of the body expression tree of an R function.
The visitation length can be interpreted as the size of the expression obtained by substituting all
inner functions by their function bodies (see "Crossover Bias in Genetic Programming", Maarten Keijzer
and James Foster).

}
\arguments{\item{expr}{An R expression.}
\item{func}{An R function.}
}
\alias{funcDepth}
\alias{exprSize}
\alias{funcSize}
\alias{exprVisitationLength}
\alias{exprVisitationLengthRecursive}
\alias{funcVisitationLength}

