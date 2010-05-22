## recombination.r
##   - Functions for recombining GP individuals (e.g. by crossover)
##
## RGP - a GP system for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Select random childs or subtrees of an expression
##'
##' \code{randchild} returns a uniformly random direct child of an expression.
##' \code{randsubtree} returns a uniformly random subtree of an expression. Note that this subtree
##' must not be a direct child.
##
##' @param expr The expression to select random childs or subtrees from.
##' @param subtreeprob The probability for \code{randsubtree} to select a certain subtree instead
##' of searching further via an recursive call.
##'
##' @rdname randomExpressionChilds
##' @export
randchild <- function(expr)
  expr[[ceiling(runif(1, 1, length(expr)))]]

##' @rdname randomExpressionChilds
##' @export
randsubtree <- function(expr, subtreeprob = 0.1)
  if (is.call(expr) && runif(1) > subtreeprob) randsubtree(randchild(expr), subtreeprob) else expr

##' Random crossover of functions and expressions
##'
##' Replace a random subtree of \code{func1} (\code{expr1}) with a random subtree of
##' \code{func2} (\code{expr2}) and return the resulting function (expression), i.e.
##' the modified \code{func1} (\code{expr1}).
##' \code{crossoverexpr} handles crossover of expressions instead of functions.
##' \code{crossoverTyped} and \code{crossoverexprTyped} only exchage replace subtress
##' if the sTypes of their root nodes match.
##'
##' @param expr1 The first parent R expression.
##' @param func1 The first parent R function.
##' @param expr2 The second parent R expression.
##' @param func2 The second parent R function.
##' @param crossoverprob The probability of crossover at each node of the first parent
##'   function (expression).
##' @return The child function (expression).
##'
##' @rdname expressionCrossover
##' @export
crossover <- function(func1, func2, crossoverprob = 0.1) {
  child <- new.function() 
  formals(child) <- formals(func1)
  body(child) <- crossoverexpr(body(func1), body(func2), crossoverprob)
  child
}

##' @rdname expressionCrossover
##' @export
crossoverexpr <- function(expr1, expr2, crossoverprob) {
  newexpr1 <- expr1
  if (is.call(expr1)) {
    crossoverindex <- ceiling(runif(1, 1, length(expr1)))
    if (runif(1) <= crossoverprob) { # do crossover here
      newexpr1[[crossoverindex]] <- randsubtree(expr2, crossoverprob)
    } else { # try to do crossover somewhere below in this tree
      newexpr1[[crossoverindex]] <- crossoverexpr(expr1[[crossoverindex]], expr2, crossoverprob)
    }
  }
  newexpr1
}

##' @rdname expressionCrossover
##' @export
crossoverTyped <- function(func1, func2, crossoverprob = 0.1) {
  child <- new.function() 
  formals(child) <- formals(func1)
  body(child) <- crossoverexprTyped(body(func1), body(func2), crossoverprob)
  child
}

##' @rdname expressionCrossover
##' @export
crossoverexprTyped <- function(expr1, expr2, crossoverprob) {
  newexpr1 <- expr1
  if (is.call(expr1)) {
    crossoverindex <- ceiling(runif(1, 1, length(expr1)))
    if (runif(1) <= crossoverprob) { # do crossover here
      ## select a random subtree of expr2 as a replacement expression...
      replacementExpression <- randsubtree(expr2, crossoverprob)
      ## collect the range types of the crossover points...
      crossoverExpressionRangeType <- rangeTypeOfType(sType(newexpr1[[crossoverindex]]))
      replacementExpressionRangeType <- rangeTypeOfType(sType(replacementExpression))
      ## only do crossover if the range types of the crossover points match...
      if (identical(replacementExpressionRangeType, crossoverExpressionRangeType)) {
        newexpr1[[crossoverindex]] <- replacementExpression
      }
    } else { # try to do crossover somewhere below in this tree
      newexpr1[[crossoverindex]] <- crossoverexprTyped(expr1[[crossoverindex]], expr2, crossoverprob)
    }
  }
  newexpr1
}
