\name{expressionTransformation}
\alias{MapExpressionNodes}
\title{Common higher-order functions for transforming R expressions...}
\usage{MapExpressionNodes(f, expr)
FlattenExpression(expr)
AllExpressionNodes(p, expr)
AnyExpressionNode(p, expr)
}
\description{Common higher-order functions for transforming R expressions}
\details{\code{MapExpressionNodes}: \code{MapExpressionNodes} transforms an expression \code{expr} by
replacing every node in the tree with the result of applying a function
\code{f}. It is a variant of \code{\link{Map}} for expression trees.
\code{expr}.
\code{AllExpressionNodes} checks if all nodes in the tree of \code{expr}
satisfy the predicate \code{p} (\code{p} returns \code{TRUE} for every node).
This function short-cuts returning \code{FALSE} as soon as a node that
does not satisfy \code{p} is encountered.
\code{AnyExpressionNode} checks if any node in the tree of \code{expr}
satisfies the predicate \code{p}. This function short-cuts returning
\code{TRUE} as soon as a node that satisfies \code{p} is encountered.

}
\arguments{\item{f}{The function to apply.}
\item{p}{The predicate to check.}
\item{expr}{The expression to transformed.}
}
\alias{FlattenExpression}
\alias{AllExpressionNodes}
\alias{AnyExpressionNode}

