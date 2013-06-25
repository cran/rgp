\name{commonSubexpressions}
\alias{commonSubexpressions}
\alias{differingSubexpressions}
\alias{NCSdist}
\alias{normalizedNumberOfCommonSubexpressions}
\alias{normalizedSizeWeightedNumberOfCommonSubexpressions}
\alias{normInducedFunctionDistance}
\alias{normInducedTreeDistance}
\alias{numberOfCommonSubexpressions}
\alias{numberOfDifferingSubexpressions}
\alias{sizeWeightedNumberOfCommonSubexpressions}
\alias{sizeWeightedNumberOfDifferingSubexpressions}
\alias{SNCSdist}
\alias{trivialMetric}
\title{Similarity and Distance Measures for R Functions and Expressions}
\usage{
  commonSubexpressions(expr1, expr2)

  numberOfCommonSubexpressions(expr1, expr2)

  normalizedNumberOfCommonSubexpressions(expr1, expr2)

  NCSdist(expr1, expr2)

  sizeWeightedNumberOfCommonSubexpressions(expr1, expr2)

  normalizedSizeWeightedNumberOfCommonSubexpressions(expr1,
    expr2)

  SNCSdist(expr1, expr2)

  differingSubexpressions(expr1, expr2)

  numberOfDifferingSubexpressions(expr1, expr2)

  sizeWeightedNumberOfDifferingSubexpressions(expr1, expr2)

  trivialMetric(a, b)

  normInducedTreeDistance(norm,
    labelDistance = trivialMetric,
    distanceFoldOperator = NULL)

  normInducedFunctionDistance(norm,
    labelDistance = trivialMetric,
    distanceFoldOperator = NULL)
}
\arguments{
  \item{expr1}{An R expression.}

  \item{expr2}{An R expression.}

  \item{a}{An R object.}

  \item{b}{An R object.}

  \item{norm}{A norm to derive a tree distance metric
  from.}

  \item{labelDistance}{A metric for measuring distances of
  tree node labels, i.e. function names or constants.}

  \item{distanceFoldOperator}{The operator used by
  \code{normInducedTreeDistance} to combine the measures
  subtree distances, defaults to `+`.}
}
\description{
  These functions implement several similarity and distance
  measures for R functions (i.e. their body expressions).
  TODO check and document measure-theoretic properties of
  each measure defined here TODO these distance measures
  are metrics, some of them are norm-induced metrics
  \code{commonSubexpressions} returns the set of common
  subexpressions of \code{expr1} and \code{expr2}. This is
  not a metric by itself, but can be used to implement
  several subtree-based similarity metrics. of \code{expr1}
  and \code{expr2}.
  \code{sizeWeightedNumberOfcommonSubexpressions} returns
  the number of common subexpressions of \code{expr1} and
  \code{expr2}, weighting the size of each common
  subexpression. Note that for every expression \emph{e},
  \code{sizeWeightedNumberOfcommonSubexpressions(} \emph{e}
  \code{, } \emph{e} \code{) == exprVisitationLength(}
  \emph{e} \code{)}.
  \code{normalizedNumberOfCommonSubexpressions} returns the
  ratio of the number of common subexpressions of
  \code{expr1} and \code{expr2} in relation to the number
  of subexpression in the larger expression of \code{expr1}
  and \code{expr2}.
  \code{normalizedSizeWeightedNumberOfcommonSubexpressions}
  returns the ratio of the size-weighted number of common
  subexpressions of \code{expr1} and \code{expr2} in
  relation to the visitation length of the larger
  expression of \code{expr1} and \code{expr2}.
  \code{NCSdist} and \code{SNCSdist} are distance metrics
  derived from
  \code{normalizedNumberOfCommonSubexpressions} and
  \code{normalizedSizeWeightedNumberOfCommonSubexpressions}
  respectively. \code{differingSubexpressions}, and
  code{numberOfDifferingSubexpressions} are duals of the
  functions described above, based on counting the number
  of differing subexpressions of \code{expr1} and
  \code{expr2}. The possible functions
  "normalizedNumberOfDifferingSubexpressions" and
  "normalizedSizeWeightedNumberOfDifferingSubexpressions"
  where ommited because they are always equal to
  \code{NCSdist} and \code{SNCSdist} by definition.
  \code{trivialMetric} The "trivial" metric M(a, b) that is
  0 iff a == b, 1 otherwise. \code{normInducedTreeDistance}
  Uses a norm on expression trees and a metric on tree node
  labels to induce a metric M on expression trees A and B:
  If both A and B are empty (represented as \code{NULL}),
  M(A, B) := 0. If exactly one of A or B is empty, M(A, B)
  := "the norm applied to the non-empty tree". If neither A
  or B is empty, the difference of their root node labels
  (as measured by \code{labelDistance}) is added to the sum
  of the differences of the children. The children lists
  are padded with empty trees to equalize their sizes. The
  summation operator can be changed via
  \code{distanceFoldOperator}.
  \code{normInducedFunctionDistance} Is wrapper that
  applies \code{normInducedTreeDistance} to the bodies of
  the given functions.
}

