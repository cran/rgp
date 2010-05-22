## expression_utils.r
##   - Utility functions for R expressions
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Common higher-order functions for transforming R expressions
##'
##' \code{MapExpressionNodes} transforms an expression \code{expr} by
##' replacing every node in the tree with the result of applying a function
##' \code{f}. It is a variant of \code{\link{Map}} for expression trees.
##  \code{FlattenExpression} returns a list of all nodes in an expression
##' \code{expr}.
##' \code{AllExpressionNodes} checks if all nodes in the tree of \code{expr}
##' satisfy the predicate \code{p} (\code{p} returns \code{TRUE} for every node).
##' This function short-cuts returning \code{FALSE} as soon as a node that
##' does not satisfy \code{p} is encountered.
##' \code{AnyExpressionNode} checks if any node in the tree of \code{expr}
##' satisfies the predicate \code{p}. This function short-cuts returning
##' \code{TRUE} as soon as a node that satisfies \code{p} is encountered.
##'
##' @param f The function to apply.
##' @param p The predicate to check.
##' @param expr The expression to transformed.
##'
##' @rdname expressionTransformation
MapExpressionNodes <- function(f, expr) {
  if (is.call(expr)) {
    oldfunc <- expr[[1]]
    newfunc <- f(oldfunc)
    as.call(append(newfunc, Map(function(e) MapExpressionNodes(f, e), expr[-1])))
  } else {
    f(expr)
  }
}

##' @rdname expressionTransformation
FlattenExpression <- function(expr) {
  if (is.call(expr)) {
    func <- expr[[1]]
    c(list(func), Map(FlattenExpression, expr[-1]), recursive = TRUE)
  } else {
    list(expr)
  }
}

##' @rdname expressionTransformation
AllExpressionNodes <- function(p, expr) {
  if (is.call(expr)) {
    if (!p(expr[[1]])) return(FALSE) # check function
    if (length(expr) >= 2) { # check arguments recursively...
      for (i in 2:length(expr)) {
        if (!AllExpressionNodes(p, expr[[i]])) return(FALSE) # short-cut
      }
    }
    TRUE
  } else {
    p(expr)
  }
}

##' @rdname expressionTransformation
AnyExpressionNode <- function(p, expr) {
  if (is.call(expr)) {
    if (p(expr[[1]])) return(TRUE) # check function
    if (length(expr) >= 2) { # check arguments recursively...
      for (i in 2:length(expr)) {
        if (AnyExpressionNode(p, expr[[i]])) return(TRUE) # short-cut
      }
    }
    FALSE
  } else {
    p(expr)
  }
}

##' Functions for decomposing and recombining R expressions
##'
##' \code{subExpressions} returns a list of all subexpressions (subtrees) of an
##' expression \code{expr}.
##'
##' @param expr An R expression.
##'
##' @rdname expressionComposing
subexpressions <- function(expr)
  if (is.call(expr)) {
    c(expr, Map(subexpressions, expr[-1]), recursive = TRUE)
  } else expr
