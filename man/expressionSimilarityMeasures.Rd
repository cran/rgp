\name{expressionSimilarityMeasures}
\alias{commonSubexpressions}
\title{Similarity and distance measures for R functions and expressions...}
\usage{commonSubexpressions(expr1, expr2)
numberOfCommonSubexpressions(expr1, expr2)
normalizedNumberOfCommonSubexpressions(expr1, expr2)
NCSdist(expr1, expr2)
sizeWeightedNumberOfCommonSubexpressions(expr1, expr2)
normalizedSizeWeightedNumberOfCommonSubexpressions(expr1, expr2)
SNCSdist(expr1, expr2)
differingSubexpressions(expr1, expr2)
numberOfDifferingSubexpressions(expr1, expr2)
sizeWeightedNumberOfDifferingSubexpressions(expr1, expr2)
}
\description{Similarity and distance measures for R functions and expressions}
\details{\code{commonSubexpressions}: These functions implement several similarity and distance measures for R functions
(i.e. their body expressions).
TODO check and document measure-theoretic properties of each measure defined here
\code{commonSubexpressions} returns the set of common subexpressions of \code{expr1}
and \code{expr2}. This is not a measure itself, but can be used to implement
several subtree-based similarity measures.
of \code{expr1} and \code{expr2}.
\code{sizeWeightedNumberOfcommonSubexpressions} returns the number of common
subexpressions of \code{expr1} and \code{expr2}, weighting the size of each common
subexpression. Note that for every expression \emph{e},
\code{sizeWeightedNumberOfcommonSubexpressions(} \emph{e} \code{, } \emph{e}
\code{) == exprVisitationLength(} \emph{e} \code{)}.
\code{normalizedNumberOfCommonSubexpressions} returns the ratio of the number of
common subexpressions of \code{expr1} and \code{expr2} in relation to the number
of subexpression in the larger expression of \code{expr1} and \code{expr2}.
\code{normalizedSizeWeightedNumberOfcommonSubexpressions} returns the ratio of
the size-weighted number of common subexpressions of \code{expr1} and \code{expr2}
in relation to the visitation length of the larger expression of \code{expr1} and
\code{expr2}.
\code{NCSdist} and \code{SNCSdist} are distance measures derived from
\code{normalizedNumberOfCommonSubexpressions} and
\code{normalizedSizeWeightedNumberOfCommonSubexpressions} respectively.
\code{differingSubexpressions}, and code{numberOfDifferingSubexpressions}
are duals of the functions described above, based on counting the number of
differing subexpressions of \code{expr1} and \code{expr2}. The possible functions
"normalizedNumberOfDifferingSubexpressions" and
"normalizedSizeWeightedNumberOfDifferingSubexpressions" where ommited because they
are always equal to \code{NCSdist} and \code{SNCSdist} by definition.

}
\arguments{\item{expr1}{An R expression.}
\item{expr2}{An R expression.}
}
\alias{numberOfCommonSubexpressions}
\alias{normalizedNumberOfCommonSubexpressions}
\alias{NCSdist}
\alias{sizeWeightedNumberOfCommonSubexpressions}
\alias{normalizedSizeWeightedNumberOfCommonSubexpressions}
\alias{SNCSdist}
\alias{differingSubexpressions}
\alias{numberOfDifferingSubexpressions}
\alias{sizeWeightedNumberOfDifferingSubexpressions}

