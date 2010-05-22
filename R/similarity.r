## similarity.r
##   - Similarity and distance measures for R functions and expressions
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Patrick Koch, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Similarity and distance measures for R functions and expressions
##'
##' These functions implement several similarity and distance measures for R functions
##' (i.e. their body expressions).
##' TODO check and document measure-theoretic properties of each measure defined here
##' \code{commonSubexpressions} returns the set of common subexpressions of \code{expr1}
##' and \code{expr2}. This is not a measure itself, but can be used to implement
##' several subtree-based similarity measures.
## '\code{numberOfcommonSubexpressions} returns the number of common subexpressions
##' of \code{expr1} and \code{expr2}.
##' \code{sizeWeightedNumberOfcommonSubexpressions} returns the number of common
##' subexpressions of \code{expr1} and \code{expr2}, weighting the size of each common
##' subexpression. Note that for every expression \emph{e},
##' \code{sizeWeightedNumberOfcommonSubexpressions(} \emph{e} \code{, } \emph{e}
##' \code{) == exprVisitationLength(} \emph{e} \code{)}.
##' \code{normalizedNumberOfCommonSubexpressions} returns the ratio of the number of
##' common subexpressions of \code{expr1} and \code{expr2} in relation to the number
##' of subexpression in the larger expression of \code{expr1} and \code{expr2}.
##' \code{normalizedSizeWeightedNumberOfcommonSubexpressions} returns the ratio of
##' the size-weighted number of common subexpressions of \code{expr1} and \code{expr2}
##' in relation to the visitation length of the larger expression of \code{expr1} and
##' \code{expr2}.
##' \code{NCSdist} and \code{SNCSdist} are distance measures derived from
##' \code{normalizedNumberOfCommonSubexpressions} and
##' \code{normalizedSizeWeightedNumberOfCommonSubexpressions} respectively.
##' \code{differingSubexpressions}, and code{numberOfDifferingSubexpressions}
##' are duals of the functions described above, based on counting the number of
##' differing subexpressions of \code{expr1} and \code{expr2}. The possible functions
##' "normalizedNumberOfDifferingSubexpressions" and
##' "normalizedSizeWeightedNumberOfDifferingSubexpressions" where ommited because they
##' are always equal to \code{NCSdist} and \code{SNCSdist} by definition.
##'
##' @param expr1 An R expression.
##' @param expr2 An R expression.
##'
##' @rdname expressionSimilarityMeasures
##' @export
commonSubexpressions <- function(expr1, expr2)
  intersect(subexpressions(expr1), subexpressions(expr2))

##' @rdname expressionSimilarityMeasures
##' @export
numberOfCommonSubexpressions <- function(expr1, expr2)
  length(commonSubexpressions(expr1, expr2))

##' @rdname expressionSimilarityMeasures
##' @export
normalizedNumberOfCommonSubexpressions <- function(expr1, expr2)
  numberOfCommonSubexpressions(expr1, expr2) / max(exprSize(expr1), exprSize(expr2))

##' @rdname expressionSimilarityMeasures
##' @export
NCSdist <- function(expr1, expr2)
  1 - normalizedNumberOfCommonSubexpressions(expr1, expr2)

##' @rdname expressionSimilarityMeasures
##' @export
sizeWeightedNumberOfCommonSubexpressions <- function(expr1, expr2) {
  weightedCommonSubexpressions <- sapply(commonSubexpressions(expr1, expr2), exprSize)
  if (identical(weightedCommonSubexpressions, list())) 0 else sum(weightedCommonSubexpressions)
}

##' @rdname expressionSimilarityMeasures
##' @export
normalizedSizeWeightedNumberOfCommonSubexpressions <- function(expr1, expr2) {
  largerExpr <- if (exprSize(expr1) >= exprSize(expr2)) expr1 else expr2
  sizeWeightedNumberOfCommonSubexpressions(expr1, expr2) / exprVisitationLength(largerExpr)
}

##' @rdname expressionSimilarityMeasures
##' @export
SNCSdist <- function(expr1, expr2)
  1 - normalizedSizeWeightedNumberOfCommonSubexpressions(expr1, expr2)

##' @rdname expressionSimilarityMeasures
##' @export
differingSubexpressions <- function(expr1, expr2) {
  expr1subs <- subexpressions(expr1)
  expr2subs <- subexpressions(expr2)
  setdiff(union(expr1subs, expr2subs), intersect(expr1subs, expr2subs))
}

##' @rdname expressionSimilarityMeasures
##' @export
numberOfDifferingSubexpressions <- function(expr1, expr2)
  length(differingSubexpressions(expr1, expr2))

##' @rdname expressionSimilarityMeasures
##' @export
sizeWeightedNumberOfDifferingSubexpressions <- function(expr1, expr2) {
  weightedDifferingSubexpressions <- sapply(differingSubexpressions(expr1, expr2), exprSize)
  if (identical(weightedDifferingSubexpressions, list())) 0 else sum(weightedDifferingSubexpressions)
}
