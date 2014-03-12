\name{exprDepth}
\alias{exprCount}
\alias{exprDepth}
\alias{exprLeaves}
\alias{exprSize}
\alias{exprVisitationLength}
\alias{fastExprVisitationLength}
\alias{fastFuncVisitationLength}
\alias{funcCount}
\alias{funcDepth}
\alias{funcLeaves}
\alias{funcSize}
\alias{funcVisitationLength}
\title{Complexity measures for R functions and expressions}
\usage{
exprDepth(expr)

funcDepth(func)

exprSize(expr)

exprLeaves(expr)

exprCount(expr, predicate = function(node) TRUE)

funcSize(func)

funcLeaves(func)

funcCount(func, predicate = function(node) TRUE)

exprVisitationLength(expr, intermediateResults = FALSE)

fastExprVisitationLength(expr, intermediateResults = FALSE)

funcVisitationLength(func, intermediateResults = FALSE)

fastFuncVisitationLength(func, intermediateResults = FALSE)
}
\arguments{
  \item{expr}{An R expression.}

  \item{func}{An R function.}

  \item{predicate}{An R predicate (function with range type
  \code{logical}).}

  \item{intermediateResults}{Whether to return complexity
  measures for all subtrees also.}
}
\description{
\code{exprDepth} returns the depth of the tree
representation ("exression tree") of an R expression.
\code{funcDepth} returns the tree depth of the body
expression of an R function. \code{exprSize} returns the
number of nodes in the tree of an R expression.
\code{exprLeaves} returns the number of leave nodes in the
tree of an R expression. \code{exprCount} returns the
number of tree nodes in an R expression matching a given
predicate. \code{funcSize} returns the number of nodes in
the body expression tree of an R function.
\code{funcLeaves} returns the number of leave nodes in the
body expression tree of an R function. \code{funcCount}
returns the number of nodes in an R function body
expression matching a given predicate.
\code{exprVisitationLength} returns the visitation length
of the tree of an R expression. The visitation length is
the total number of nodes in all possible subtrees of a
tree. \code{funcVisitationLength} returns the visitation
length of the body expression tree of an R function.
\code{fastExprVisitationLength} and
\code{fastFuncVisitationLength} are variants written in
optimized C code. The visitation length can be interpreted
as the size of the expression obtained by substituting all
inner functions by their function bodies (see "Crossover
Bias in Genetic Programming", Maarten Keijzer and James
Foster).
}

